GetNodeBase <- function(org.file){
    file.content <- readLines(org.file)
    heading.lines <- grep("^\\*{1, } ", file.content) ## start with at least 1 starts, and then a space, 
    headings <- stringr::str_trim(file.content[heading.lines])
    n.stars <- sapply(headings, function(x) {
        stars <- str_split(x, " ")[[1]][1]
        str_length(stars)
    })
    headings <- str_replace_all(headings, pattern = "^\\*{1,} ", "") ## remove stars 
    node.base <- data.table(node.id = seq_len(length(headings)),
                            level = n.stars,
                            headline = headings,
                            start.line.num = heading.lines,
                            end.line.num = c(heading.lines[-1] - 1, length(file.content)))
    setkey(node.base, node.id)
    node.base
}
