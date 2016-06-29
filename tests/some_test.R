library(devtools)
load_all("orgR")
example.file.path <- "example.org"
file.content <- readLines(example.file.path)
node.base <- GetNodeBase()
setkey(node.base, node.id)


## clock.table <- GetClockTable(example.file.path)

bug <- NA 
clock.table2 <- rbindlist(lapply(node.base$node.id, function(i.id){
    bug <<- i.id 
    line.range <- node.base[J(i.id), c(start.line.num, end.line.num)]
    node.content <- file.content[line.range[1] : line.range[2]]
    clock.entry.lines <- grep("CLOCK: \\[", node.content)
    if (length(clock.entry.lines) == 0){
        cat("\nNo clock entries for node with id ", i.id)
                                        # return(data.table(0, 0, i.id))
        return(data.table())
    }
    clock.entries <- node.content[clock.entry.lines]
    clock.closed <- grepl("--", clock.entries)
    if (!all(clock.closed)){
        cat("\nUnclosed Clock Entries for node with id ", i.id)
        return(data.table())
    }
    clock.table <- as.data.table(ToISOdate(clock.entries))
    clock.table[, node.id := i.id]
    return(clock.table)
}))
clock.table2[, duration := end.time - start.time]
setkey(clock.table2, node.id)
clock.table2 <- clock.table2[node.base, allow = TRUE]
clock.table2[, c("start.line.num", "end.line.num") := NULL]
setcolorder(clock.table2, c("node.id", "level", "headline", "start.time", "end.time", "duration"))





visu.ts.plot <- function(Clock, id) {
    children.ids <- searchChildren(node.base, id, 100)
    if (is.null(children.ids)) {
        return()
    }
    gg.df <- Clock[J(c(id, children.ids)), ]
    ggplot(gg.df, aes(start.time, duration)) + geom_point()
}

visu.hist.plot <- function(Clock, id) {
    children.ids <- searchChildren(node.base, id, 100)
    if (is.null(children.ids)) {
        return()
    }
    gg.df <- Clock[J(c(id, children.ids)), ]
    ggplot(gg.df, aes(duration)) + geom_histogram()
}

visu.pie.plot <- function(Clock, id, drillDown.level = 1) {
    gg.df <- drillDown.Clock(clock.table2, id, drillDown.level)
    if (is.null(gg.df))
        return()
    p <- ggpie(gg.df, "headline", "sum") + labs(title = Clock[J(id), unique(headline)])
    return(p)
}


compare.ts.plot <- function(Clock, ids, by.col = TRUE) {
    gg.df <- rbindlist(lapply(ids, function(id) {
        p <- visu.ts.plot(Clock, id)
        if (is.null(p))
            return()
        data.table(p$data[, list(start.time, duration)], headline = Clock[J(id), unique(headline)])
    }))
    if (by.col) {
        p <- ggplot(gg.df, aes(start.time, duration)) + geom_point(aes(col = headline))
    } else {
        p <- ggplot(gg.df, aes(start.time, duration)) + geom_point() + facet_wrap(~headline)
    }
    return(p)
}

compare.hist.plot <- function(Clock, ids, by.col = TRUE) {
   gg.df <- rbindlist(lapply(ids, function(id) {
        p <- visu.hist.plot(Clock, id)
        if (is.null(p))
            return()
        data.table(p$data[, list(start.time, duration)], headline = Clock[J(id), unique(headline)])
    }))
   if (by.col){
       p <- ggplot(gg.df, aes(duration)) + geom_histogram(aes(fill = headline))
   } else {
       p <- ggplot(gg.df, aes(duration)) + geom_histogram() + facet_wrap(~headline)
   }
   return(p)
}

compare.pie.plot <- function(Clock, ids, by.col = TRUE, drillDOwn.level = 1)  {
    p.list <- lapply(ids, function(id) visu.pie.plot(Clock, id, drillDown.level))
    return(do.call(arrangeGrob, p.list))
}
