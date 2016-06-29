searchParents <- function(node.base, id, parents.level = 1L){
    id.level <- node.base[J(id), level]
    target.level <- id.level - parents.level
    if (target.level < min(node.base$node.id)) {
        cat("\nOut of range: searchParents()")
        return()
    }
    ## search backward, the first one that less than the id.level is the parent
    tmp <- node.base[level == target.level,]
    tmp2 <- rev(tmp$node.id)
    ind <- tmp2 < id
    if (!any(ind)){
        cat("\nNo parent found for node.id: ", id)
        return()
    }
    parent.node.id <- tmp2[which(ind)[1]]
    return(parent.node.id)
}

searchChildren <- function(node.base, id, children.level = 1L){
    id.level <- node.base[J(id), level]
    target.level <- seq(id.level + 1, id.level + children.level)
    if (min(target.level) > max(node.base$level)) {
        cat("\nOut of range: searchChildren()")
        return()
    }
    ## ROI:
    next.sib.id <- node.base[level == id.level & node.id > id][1, node.id]
    if (is.na(next.sib.id)) {
        tmp <- node.base[J((id+1):max(node.id))]
    } else if (next.sib.id == id + 1) {
        cat("\nNo children for node.id: ", id)
        return()
    } else {
        tmp <- node.base[J(seq(id+1, next.sib.id - 1))]
    }
    ## lists all nodes below current level
    children.node.id <- tmp[level %in% target.level & node.id > id, node.id]
    return(children.node.id)
}



searchSiblings <- function(node.base, id){
    ## same level & same parents
    id.level <- node.base[J(id), level]
    id.parents <- searchParents(node.base, id, 1L)
    if (is.null(id.parents)) {
        cat("\nNo parents, therefore no siblings")
        return()
    }
    potential.sib.id <- node.base[!J(id)][level == id.level, node.id]
    tmp <- sapply(potential.sib.id, function(x) searchParents(node.base, x, 1L))
    sib.ind <- tmp == id.parents
    if (!any(sib.ind)) {
        cat("\nNo siblings")
        return()
    }
    true.sib.id <- potential.sib.id[sib.ind]
    return(true.sib.id)
}

searchFamily <- function(node.base, id, parents.level = 1L, children.level = 1L) {
    parents <- searchParents(node.base, id)
    children <- searchChildren(node.base, id, children.level)
    siblings <- searchSiblings(node.base, id)
    return(c(id,
             parents,
             children,
             siblings))
}

formulateFamilyTree <- function(node.base, ancestor.level = 1, children.level = 1) {
    node.base <- copy(node.base)
    ancestor.node <- node.base[level == ancestor.level, ]
    familyTree <- lapply(seq_len(nrow(ancestor.node)), function(i) {
        print(i)
        ancestor.node.id <- ancestor.node[i, node.id]
        children.node.id <- searchChildren(node.base, ancestor.node.id, children.level)
        if (is.null(children.node.id)) return()
        data.table(ancestor.node.id, children.node.id)
    })
    null.idx <- sapply(familyTree, is.null)
    familyTree[!null.idx]
    ## return(familyTree)    
}

addFamilyName <- function(node.base, familyTree){
    "
* Rest
** Sleep
** Nap
** Sleep

--> (Rest, Sleep),
--> (Rest, Nap),
--> (Rest, Sleep)
"
    node.base <- copy(node.base)
    res <- lapply(seq_along(familyTree), function(i){
        print(i)
        children.ids <- familyTree[[i]]$children.node.id
        ancestor.id <- unique(familyTree[[i]]$ancestor.node.id)
        ancestor.headline <- node.base[J(ancestor.id), headline]
        node.base[J(c(ancestor.id, children.ids)), ancestor.headlines := ancestor.headline]
    })
                                        # return(node.base)
    rbindlist(res)
}
