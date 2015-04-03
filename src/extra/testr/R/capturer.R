#' @title Decorates function to capture calls and return values 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' Replaces the function by decorated function in the global environment
#' @param func function name as a character string
#' @export 
#' @seealso WriteCapInfo Decorate
#'
Decorate <- function(func, envir = .GlobalEnv){
  if (class(func) == "function"){
    fname <- as.character(substitute(func))
  } else if (class(func) == "character"){
    fname <- func
  } else {
    stop("wrong argument type!")
  }   
  if (is_s3_generic(fname)) return(NULL);
  write.call <- call("WriteCapInfo", fname, quote(sys.frame(-4)))
  tc <- call('trace', 
             fname, 
             quote(write.call),
             print=quote(testrOptions('verbose')))
  eval(tc)
  cache$decorated <- c(cache$decorated, fname)
} 

#' @title Undecorate function
#' 
#' Reset previously decorate function
#' @param func function name as a character string
#' @export 
#' @seealso WriteCapInfo Decorate
#'
Undecorate <- function(func) {
  if (class(func) == "functionWithTrace"){
    fname <- as.character(substitute(func))
  } else if (class(func) == "character"){
    fname <- func
  } else {
    stop("wrong argument type!")
  }  
  ind <- which(fname %in% cache$decorated)
  if (length(ind) == 0)
    stop("Function was not decorated!")
  do.call(untrace, list(fname))
  cache$decorated <- cache$decorated[-ind]
}

#' @title Write down capture information 
#' 
#' This function is respinsible for writing down capture information for decorated function calls.
#' @param fname function name
#' @param args.env environment to read arguments to function call from
#' @seealso Decorate
#' @useDynLib testr
#' @importFrom Rcpp evalCpp
#' @export
#' 
WriteCapInfo <- function(fname, args.env){
  if (cache$writing.down)
    return(NULL);
  .Call('testr_WriteCapInfo_cpp', PACKAGE = 'testr', fname, args.env)
}

#' @title Setup information capturing for list of function
#' 
#' This function is respinsible for setting up capturing for functions
#' 
#' @param flist function or list of functions to turn on capturing for. List should be only as character.
#' @seealso Decorate
#' @export
SetupCapture <- function(flist){
  set.cache('writing.down', TRUE)
  for (func in flist){
    if (EligibleForCapture(func)){
      Decorate(func)
    }
  }  
  set.cache('writing.down', FALSE)
}

#' @title Check if function is eligible for wrapping to capture arguments and return values
#' 
#' This function checks that supplied function for capture is not a keyword, operator or in the blacklist (functions like rm, .GlobalEnv, etc.)
#' This is an internal function and is supposed to be used in SetupCapture
#' @param func function name to check
#' @return TRUE/FALSE if can be captured or not
#' @seealso SetupCapture
EligibleForCapture <- function(func){
  return (!length(getAnywhere(func)$objs) == 0 &&
            class(getAnywhere(func)[1]) == "function" &&
            !func %in% blacklist &&
            !func %in% operators &&
            !func %in% keywords &&
            !func %in% sys &&
            !func %in% env)
}

#' @title Setup capture of builtin functions
#' 
#' Sets up capturing of builtin functions
#' @param internal wheather only internals should be captured, or all builtins
#' @param functions list of functions to be decorate
#' @param indexes specific indexes from functions vector
#' @seealso SetupCapture
#' @export
BeginBuiltinCapture <- function(internal = FALSE, functions = builtins(internal), indexes){
  if (missing(indexes))
    SetupCapture(functions)
  else
    SetupCapture(functions[indexes])
}

#' @title Clear decoration
#' 
#' Clear anything previously decorate
#' @seealso Undecorate
#' @export
ClearDecoration <- function() {
  for (fname in cache$decorated)
    Undecorate(fname)
}

GetArgs <- function(dotsE) {
  res <- .Call('testr_GetArgs', PACKAGE = 'testr', dotsE)
  res
}

is_s3_generic <- function(fname) {
  f <- get(fname, env = parent.frame(), mode = "function")
  if (is.null(body(f))) return(FALSE)
  uses <- findGlobals(f, merge = FALSE)$functions
  any(uses == "UseMethod")
}
