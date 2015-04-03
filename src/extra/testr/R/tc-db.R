#' @title Orginize test case files
#' 
#' This function is respinsible for orginizing test cases files properly (naming and one file per file)
#' @param root folder to be processed
#' @param res.dir resulting folder. If this argument is missing than files in original folder will be replaced
#' @export
#' 
OrginizeTestCases <- function(root, res.dir = tempdir()){
  files <- GetAllFiles(root)
  if (missing(res.dir)) {
    copy.back <- TRUE
    res.dir <- tempdir()
  } else {
    copy.back <- FALSE
    if (!file.exists(res.dir))
      dir.create(res.dir)
  }
  cache <- new.env();
  for (filename in files) {
    
    cat(filename, "\n")
    function.name <- ExtractFunctionName(filename)   
    function.count <- cache[[function.name]]
    if (is.null(function.count)){
      function.count <- 0
      dir.create(paste(res.dir, function.name, sep="/"))      
    }
    lines <- readLines(filename);
    tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
    if (length(tests.starts) == 0) tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
    tests.ends <- grep("^[ ]*$", lines)
    if (length(tests.ends) == 0) tests.ends <- grep("^[ ]*$", lines)
    for (i in 1:length(tests.starts)){    
      function.count <- function.count + 1
      new.file.name <- sprintf("%s/%s/tc_%s_%d.R", res.dir, function.name, function.name, function.count)
      writeLines(lines[tests.starts[i]:tests.ends[i]], new.file.name)
    }
    cache[[function.name]] <- function.count
  }
  if (copy.back){
    unlink(root, TRUE, TRUE)
    system(sprintf("mv %s %s", res.dir, root))
  }
}

#' @title Properly add test cases to the data base
#' 
#' This function is respinsible for adding test cases to a database with proper naming and numbering
#' @param db.root testcase database folder
#' @param tc.root folder of testcases to be added
#' @export
#'
AddTCsDB <- function(db.root, tc.root){
  db.files <- vector()
  # test.folder sanity check
  if (file.exists(db.root)){
    if (!file.info(db.root)$isdir) stop("Specified location of tests is not a folder")
    db.files <- GetAllFiles(db.root, full.names = F)
    db.files <- sapply(db.files, function(x) ExtractFunctionName(x, FALSE))
    db.files <- unique(db.files)
  } else {
    dir.create(db.root)
  }
  
  tc.files <- GetAllFiles(tc.root)
  # cache for storing information about functions (code and test case count)
  function.cache <- list()
  
  for (filename in tc.files) {
    function.name <- ExtractFunctionName(filename)
    function.count <- function.cache[[function.name]]
    
    if (is.null(function.count)) {
      if (function.name %in% db.files) {
        function.count <- length(list.files(file.path(db.root, function.name)))
      } else {
        dir.create(file.path(db.root, function.name))
        function.count <- 0
      }
    }
    lines <- readLines(filename)
    tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
    if (length(tests.starts) == 0) tests.starts <- head(c(1, grep("^[ ]*$", lines) + 1), -1)
    tests.ends <- grep("^[ ]*$", lines)
    if (length(tests.ends) == 0) tests.ends <- grep("^[ ]*$", lines)
    for (i in 1:length(tests.starts)){    
      function.count <- function.count + 1
      new.file.name <- sprintf("%s/%s/tc_%s_%d.R", db.root, function.name, function.name, function.count)
      writeLines(lines[tests.starts[i]:tests.ends[i]], new.file.name)
    }
    function.cache[[function.name]] <- function.count
  }
}

#' @title Remove duplicate testcases
#' 
#' This function is respinsible for removing testcases with the same arguments
#' @param root testcase folder to be processed
#' @export
#'
RemoveDuplicates <- function(root){
  TestGetArgs <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
    as.list(substitute(code))[2]   
  }
  if (file.info(root)$isdir){
    files <- list.files(root, pattern=".[rR]$", recursive = TRUE, all.files = TRUE, full.names = TRUE) 
  } 
  args.cache <- new.env() 
  temp.env <- new.env()
  for (filename in files){
    function.name <- ExtractFunctionName(filename)
    args.list <- args.cache[[function.name]]
    if (is.null(args.list)) {
      args.list <- list()
    }
    temp.env$test <- TestGetArgs
    with(temp.env, argv <- source(filename, local = TRUE)$value)
    argv <- temp.env$argv
    repeated <- FALSE
    for (saved.argv in args.list){
      if (identical(all.equal(argv, saved.argv), TRUE)) {
        repeated <- TRUE
        break
      }
    }
    if (!repeated) {
      if (is.null(argv))
        args.list <- c(args.list, list(NULL))
      else
        args.list <- c(args.list, argv)
      args.cache[[function.name]] <- args.list
    } else {
      file.remove(filename)
    }
  }
}

#' @title Get all files with specific pattern
#' 
#' This function is respinsible for leturning all files from specified folder
#' @param root input folder
#' @param pattern pattern of files to be searched for
#' @param full.names if full path to files should be returned
#'
GetAllFiles <- function(root, pattern = ".[rR]$", full.names = T){
  if (file.info(root)$isdir){
    files <- list.files(root, pattern=pattern, recursive = TRUE, all.files = TRUE, full.names = full.names) 
  } else {
    files <- root
  }
  files
}

#' @title Get function name without special characters
#' 
#' This function is respinsible for extractng function name from test file name and removing special characters
#' @param filename filename to be processed
#' @param modify.characters if special characters should be removed
#'
ExtractFunctionName <- function(filename, modify.characters = TRUE){
  function.name <- filename
  if (grepl(".[rR]$", filename))
    function.name <- gsub("(.*)tc_(.*)_(.*).R", "\\2", filename)
  if (function.name %in% operators) function.name <- "operators"
  if (modify.characters){
    function.name <- gsub("\\.", "", function.name)
    function.name <- gsub("<-", "assign_", function.name)
    function.name <- gsub("\\[", "extract_parentasis_", function.name)
    function.name <- gsub("\\$", "extract_dollar_", function.name)
    function.name <- gsub("\\+", "plus_", function.name)
    function.name <- gsub("\\-", "minus_", function.name)
    function.name <- gsub("&", "and_", function.name)
    function.name <- gsub("\\*", "times_", function.name)
  }
  function.name
}
