.onUnload <- function(libpath)
    library.dynam.unload("rpart", libpath)

.noGenerics <- TRUE

tree.depth <- function (nodes)
{
    depth <- floor(log(nodes, base = 2) + 1e-7)
    depth - min(depth) # doesn't seem to need as.vector.
}

string.bounding.box <- function(s)
{
    s2 <- strsplit(s, "\n")
    rows <- sapply(s2, length)
    columns <- sapply(s2, function(x) max(nchar(x, "w")))
    list(columns=columns, rows=rows)
}

node.match <- function(nodes, nodelist, leaves, print.it = TRUE)
{
    node.index <- match(nodes, nodelist, nomatch = 0)
    bad <- nodes[node.index == 0]
    if(length(bad) > 0 & print.it)
        warning("supplied nodes ", paste(bad, collapse = ","),
                " are not in this tree")
    good <- nodes[node.index > 0]
    if(!missing(leaves) && any(leaves <- leaves[node.index])) {
        warning("supplied nodes ",
                paste(good[leaves], collapse = ","), " are leaves")
        node.index[node.index > 0][!leaves]
    }
    else node.index[node.index > 0]
}

descendants <- function(nodes, include = TRUE)
{
    n <- length(nodes)
    if(n == 1) return(matrix(TRUE, 1, 1))
    ind <- 1:n
    desc <- matrix(FALSE, n, n)
    if(include) diag(desc) <- TRUE
    parents <- match((nodes %/% 2), nodes)
    lev <- floor(log(nodes, base = 2))
    desc[1, 2:n] <- TRUE
    for(i in max(lev):2) {
        desc[cbind(ind[parents[lev == i]], ind[lev == i])] <- TRUE
        parents[lev == i] <- parents[parents[lev == i]]
        lev[lev == i] <- i - 1
    }
    return(desc)
}
