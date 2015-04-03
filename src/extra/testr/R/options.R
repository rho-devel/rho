## general (temporary) storage for testr's stuff
cache           <- new.env()
cache$capture.file.number <- 0
cache$writing.down <- FALSE

cache$arguments <- list()
cache$decorated <- vector()

kCaptureFile <- "capture"
kCaptureFolder <- "capture"
kSymbPrefix <- "symb: "
kValSPrefix <- "vsym: "
kFuncPrefix <- "func: "
kArgsPrefix <- "argv: "

blacklist <- c("builtins", "rm", "source", "~", "<-", "$", "<<-", "&&", "||" ,"{", "(", 
               ".GlobalEnv", ".Internal", ".Primitive", "::", ":::", "substitute", "list", 
               ".Machine", "on.exit", "debug", "undebug",
               "withCallingHandlers", "quote", ".signalSimpleWarning", "..getNamespace", ".External", ".External2", 
               "c", "try", "NextMethod", "UseMethod",# no idea why
               "setwd", # path of capture files are relative to WD, change that
               "rawConnection", ".handleSimpleError", "tryCatch",
               "library", # something problematic
               "standardGeneric", "identity","missing",
               "options", "ls", "sys.call", "stdout", "do.call", "cat", "withVisible",
               "sprintf", "parse", "paste", 
               "textConnection", "require", "with", "get", "sink", "eval",
               "parse", "paste", "paste0", "evalq", "deparse", "exists", "environment", "conditionMessage.condition", "simpleError", "as.name",
               "attach", "attachNamespace", "lazyLoadDBexec", "lazyLoad", "lazyLoadDBfetch", "as.null.default", "asNamespace", "contributors", "close.connection",
               "close.srcfile", "close.srcfilealias", "computeRestarts", "findRestarts", "bindingIsLocked", "browserCondition", "browserSetDebug", "browserText", "closeAllConnections",
               "debugonce", "callCC", "delayedAssign", "detach", "browser", "clearPushBack", ".row_names_info", ".deparseOpts", ".makeMessage", ".libPaths", "%in%",
               "getNamespace", "isNamespace", "stdin", "stderr", "stop", "stopifnot", "structure", "local", "merge.data.frame", 
               "match", "match.arg", "typeof", "conditionCall.condition", "withRestarts", "formals",
               # for .Primitive and functions without body
               ".C", ".Call", ".External", ".External.graphics", ".External2", ".Fortran",
               "as.call", "names<-", "names", "length", "is.pairlist", "is.null", "is.list", "invisible", "class<-", "class", 
               "baseenv", "attributes<-", "as.environment", "as.character", ".Call.graphics" , 
               "length<-", "call", "attr<-", "switch", "log2", "nargs", "as.numeric",
               #                "xtfrm", "as.double","rep", "round", "max", "min",
               "attributes", "attributes<-", "is.language", 
               # errors with trace              
               "match.call", ".doTrace", "tracingState", "traceback", "trace" 
)

sys <- c('system.time','system.file','sys.status','sys.source','sys.save.image','sys.parents','sys.parent','sys.on.exit','sys.nframe','sys.load.image','sys.function','sys.frames','sys.frame','sys.calls','sys.call','R_system_version','.First.sys')
env <- c("environment", "environment<-", "parent.frame", "parent.env", "parent.env<-")
keywords <- c("while", "return", "repeat", "next", "if", "function", "for", "break")
operators <- c("(", ":", "%sep%", "[", "[[", "$", "@", "=", "[<-", "[[<-", "$<-", "@<-", "+", "-", "*", "/", 
               "^", "%%", "%*%", "%/%", "<", "<=", "==", "!=", ">=", ">", "|", "||", "&", "!")
.onLoad <- function(libname, pkgname)
{
  if (!file.exists(kCaptureFolder) || !file.info(kCaptureFolder)$isdir)
    dir.create(kCaptureFolder)
  cache$trace.folder.path <-  file.path(getwd(), kCaptureFolder)
  ## testr settings
  options('testr' = list(
    'verbose' = FALSE,
    'display.only.errors' = FALSE,
    'stop.on.error' = FALSE,
    'display.code.on.error' = FALSE,
    'file.summary' = FALSE,
    'capture.file.size' = 50 * 1000 * 1000
  ))
  require(codetools)
}

set.cache <- function(x, value){
  assign(x, value, envir = cache)
}

#' Querying/setting testr option
#'
#' To list all \code{testr} options, just run this function without any parameters provided. To query only one value, pass the first parameter. To set that, use the \code{value} parameter too.
#'
#' The following \code{testr} options are available:
#'
#' \itemize{
#'      \item \code{digits}: numeric (default: \code{2}) passed to \code{format}
#' }
#' @param o option name (string). See below.
#' @param value value to assign (optional)
#' @export
#' 
testrOptions <- function(o, value) {
  
  res <- getOption('testr')
  
  ## just querying
  if (missing(value)) {
    
    if (missing(o))
      return(res)
    
    if (o %in% names(res))
      return(res[[o]])
    
    stop('Wrong option queried.')
    
  } else {
    
    if (!o %in% names(res))
      stop(paste('Invalid option name:', o))
    
    res[[o]] <- value
    options('testr' = res)
    
  }
  
}

#' @export
testr.option <- function(x, ...) {
  mc <- match.call(testrOptions)
  mc[[1]] <- quote(testrOptions)
  eval(mc)
}
