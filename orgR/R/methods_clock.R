#### data manipulation 

subsetDuration.Clock <- function(Clock, min, max, inner = TRUE){
    ind <- Clock[, duration >= min & duration <= max]
    if (inner)
        return(Clock[ind])
    Clock[!ind]
}

subsetPeriod.Clock <- function(Clock, time1, time2, wrap = FALSE){
    ind1 <- Clock$start.time >= time1
    ind2 <- Clock$end.time <= time2
    if (wrap)
        return(Clock[ind1 & ind2])
    Clock[ind1 | ind2]
}

## period.Clock(clock.table2, quantile(clock.table2$start.time, 0.25), quantile(clock.table$start.time, 0.75))

fullRange.Clock <- function(){
    
}

#### sec: calculation 
sum.Clock <- function(Clock, by.level = 1){
    if(max(Clock$level) <= by.level)
        return()
    Clock <- copy(Clock)
    Clock <- Clock[level < by.level, duration := NA]
    this.level.ind <- Clock$level == by.level 
    code <- rle(this.level.ind)
    end <- cumsum(code$lengths)
    start <- c(1, end[-length(end)] + 1)
    n <- length(start)
    agg.Clock <- rbindlist(lapply(1:(n %/% 2), function(i) {
        ind <- start[(i-1)*2 + 1]: end[i * 2]
        Clock[ind, list(node.id = node.id[1], sum = sum(duration, na.rm=T))]
    }))
    setkey(agg.Clock, node.id)
    agg.Clock <- Clock[agg.Clock]
    unique(agg.Clock)
}

sumChildren.Clock <- function(Clock, id, children.level = max(node.base$level)){
    ## given a node.id, add up its clocking time, and also its childrens. 
    children.ids <- searchChildren(node.base, id, children.level)
    if (is.null(children.ids)) {
        cat("\nNo Children found")
        return(data.table())
    }
    children.sum <- Clock[J(children.ids), sum(duration, na.rm=T)]
    this.id.sum <- Clock[J(id), sum(duration, na.rm=T)]
    data.table(node.base[J(id)], sum = children.sum + this.id.sum)
}

drillDown.Clock <- function(Clock, id, drill.level = 1L) {
    ## given a node, add up it's children's clokcing time. 
    children.ids <- searchChildren(node.base, id, children.level)
    if (is.null(children.ids))
        return()
    sum.by.group <- rbindlist(lapply(children.ids, function(ii) sumChildren.Clock(Clock, ii)))
    attr(sum.by.group, "ancestor.name") <- node.base[J(id), headline]
    return(sum.by.group)
}


#### data visualisation 

view.Clock <- function(Clock) {
    if (is.data.table(Clock)){
        Clock <- copy(Clock)
    }
    Clock <- copy(Clock)
    Clock[, duration := as.numeric(duration)]
    p.hist.duration <- ggplot(Clock, aes(duration)) + geom_histogram()
    p.ts.duration <- ggplot(Clock, aes(x=start, y = duration)) + geom_point() + geom_smooth()
    p <- arrangeGrob(p.hist.duration,
                    p.ts.duration,
                    ncol = 2)
    return(p)
}

viewByFamily.Clock <- function(Clock, split.by.level = 1, drill.down.level = 1) {
    Clock <- copy(Clock)
    Clock[, duration := as.numeric(duration)]
    family.tree <- formulateFamilyTree(node.base, ancestor.level = split.by.level, children.level = 100)
    Clock <- addFamilyName(Clock, family.tree)
    p.hist.duration <- ggplot(Clock, aes(duration, fill = ancestor.headlines)) + geom_histogram() + scale_fill_discrete("Headline")
    p.ts.duration <- ggplot(Clock, aes(x=start.time, y = duration, col = ancestor.headlines)) + geom_point() + geom_smooth() + scale_color_discrete("Headline")
    p <- arrangeGrob(p.hist.duration,
                     p.ts.duration,
                     ncol = 2)
    return(p)
}


viewPieChart.Clock <- function(Clock, split.by.level = 1, drill.down.level = 1) {
    Clock <- copy(Clock)
    Clock[, duration := as.numeric(duration)]
    family.tree <- formulateFamilyTree(node.base, ancestor.level = split.by.level, children.level = 100)
    Clock <- addFamilyName(Clock, family.tree)
    p.pies <- lapply(seq_along(family.tree), function(i) {
        ancestor.id <- unique(family.tree[[i]]$ancestor.node.id)
        ancestor.headline <- node.base[J(ancestor.id), headline]
        k <- drillDown.Clock(clock.table2, ancestor.id)
        ggpie(k, "headline", "sum") + labs(title = ancestor.headline)
    })
    p.pies <- do.call(arrangeGrob, c(p.pies))
    return(p.pies)
}
    

