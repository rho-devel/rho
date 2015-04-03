#' @export
#' 
dputModified <- function(x) {
  res <- capture.output(dputModifiedHelper(x))
  res
}

dputModifiedHelper <- function (x){
  file <- stdout()
  control = c("keepNA", "keepInteger", "showAttributes")
  opts <- .deparseOpts(control)
  if (isS4(x)) {
    cat("new(\"", class(x), "\"\n", sep = "")
    for (n in slotNames(x)) {
      cat("    ,", n, "= ")
      if (is.language(slot.obj))
        dputModified(enquote(slot(x, n)))
      else 
        dputModified(slot(x, n))
    }
    cat(")\n")
    invisible()
  } else if(length(grep('@',capture.output(str(x)))) > 0){
    if(is.list(x) || is.vector(x)){
      is.first <- TRUE
      cat("list(\n", sep = "")
      for (i in 1:length(x)) {
        if(!is.null(names(x))){
          n <- names(x)[i]
          if(n != ''){
            if (is.first) {
              cat(n, "= ")
              is.first <- FALSE
            } else 
              cat(" , ", n, "= ")
          }
        }
        dputModified(x[[i]])
      }
      cat(")\n")
      invisible()
    } else {
      stop('S4 objects are only handled if they are contained within an S4 object or a list/vector object')
    }
  }
  else {
    if (is.language(x))
      x <- enquote(x)
    if (is.list(x) || is.vector(x)) x <- lapply(x, function(x) if(is.language(x)) enquote(x) else x)
    .Internal(dput(x, file, opts))
  }
}