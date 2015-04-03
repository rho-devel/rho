#' @title ReplaceS4 methods in the environment
#' 
#' This functions takes decorates S4 methods in the environment
#'
#' @param env environment to search for S4 methods
#' @seealso ReplaceBody
ReplaceS4 <- function(env) {
  generics <- getGenerics(env)
  
  unlist(recursive = FALSE,
         Map(generics@.Data, generics@package, USE.NAMES = FALSE,
             f = function(name, package) {
               what <- methodsPackageMetaName("T", paste(name, package, sep = ":"))
               
               table <- get(what, envir = env)
               replacer <- function(x, envir) {
                 target <- get(x, envir = table)
                 if (!is.null(target)) {
                   new.value <- testr:::ReplaceBody(name, target)
#                    rcov:::reassignInEnv(name, new.value, table
                     setMethod(name, attr(target, "target"), new.value, where = .GlobalEnv)
                 }
               }
               lapply(ls(table, all.names = TRUE), replacer)
             })
  )
}