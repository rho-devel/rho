copyright.header <- c("/*",  
                      " * This material is distributed under the GNU General Public License",
                      " * Version 2. You may review the terms of this license at", 
                      " * http://www.gnu.org/licenses/gpl-2.0.html",
                      " * ",
                      " * Copyright (c) 2014, Purdue University",  
                      " * Copyright (c) 2014, Oracle and/or its affiliates",
                      " * All rights reserved.", 
                      " */",
                      "package com.oracle.truffle.r.test.testrgen;\n", 
                      "import org.junit.*;\n",
                      "import com.oracle.truffle.r.test.*;\n")

#' @title Translate Test Cases to FastR style
#' 
#' This function is respinsible generating Java testcases for FastR. 
#' If there is a folder with already existing test cases translated from TestR to FastR, then the function will append test cases to proper files in that folder.
#' Uses unility function for extraction of test case information. Idea is that to instead of running a test when calling test function, it collects test information and converts it to FastR test
#' @param r.test.root folder with R testcases
#' @param fastr.test.folder folder with FastR test cases in Java
#' @export
#'
FastrTranslate <- function(r.test.folder, fastr.test.folder = "tests/"){
  # unility function for extraction of test case information. Replaces o
  FastrTest <- function(id, code, o = NULL, w = NULL, e = NULL, name = NULL) {
    str <- as.list(substitute(code))[-1] # extract statements from code and replace double quotes with single ones   
    str <- lapply(str, function(x) paste(deparse(x), collapse = ""))
    str <- gsub('"', "\\\\'", str)
    str <- paste(str, collapse = ';"+\n\t\t\t"', sep="")         
    res <- sprintf("\t\tassertEval(\"%s\");\n", str)  
    res 
  }
  fastr.test.files <- vector()
  # test.folder sanity check
  if (file.exists(fastr.test.folder)){
    if (!file.info(fastr.test.folder)$isdir) stop("Specified location of tests is not a folder")
    fastr.test.files <- GetAllFiles(fastr.test.folder, pattern = ".java$", full.names = F)
    fastr.test.files <- sapply(fastr.test.files, function(x) gsub("TestrGenBuiltin(.*).java", "\\1", x))
  } else {
    dir.create(fastr.test.folder)
  }
  
  r.test.files <- GetAllFiles(r.test.folder)
  # cache for storing information about functions (code and test case count)
  function.cache <- list()
  
  for (filename in r.test.files) {
    function.name <- ExtractFunctionName(filename)
    function.cache.entry <- function.cache[[function.name]]

    if (is.null(function.cache.entry)) {
      function.cache.entry <- list()
      if (function.name %in% fastr.test.files) {
        fastr.test.file <- sprintf("%s/TestrGenBuiltin%s.java", fastr.test.folder, function.name)
        fastr.test.code <- readLines(fastr.test.file)
        fastr.test.code <- fastr.test.code[1:(length(fastr.test.code) - 1)]
        
        function.cache.entry$number <- length(grep("@Test", fastr.test.code)) + 2
        function.cache.entry$code <- fastr.test.code
      } else {
        function.cache.entry$number <- 1
        function.cache.entry$code <- c(copyright.header, sprintf("// Checkstyle: stop line length check\n
                                                                 public class TestrGenBuiltin%s extends TestBase {", function.name))
      }
    }
    # for operators unify under same file
    if (function.name %in% operators){
      function.cache.entry <- function.cache[["operators"]]
      function.name <- "operators"
    }
    
    # evaluate R test file in special environment with replaces test function
    temp.env <- new.env();
    temp.env$test <- FastrTest
    with(temp.env, res <- source(filename, local = TRUE)$value)
    # create Java testcase
    test.code <- sprintf("\n\t@Test\n\tpublic void test%s%d() {\n%s\t}\n", function.name, function.cache.entry$number, temp.env$res)
    # save information back to cache
    function.cache[[function.name]]$number <- function.cache.entry$number + 1
    function.cache[[function.name]]$code <- c(function.cache.entry$code, test.code)
  }
  # generate write down information to Java testcase files
  for (function.name in names(function.cache)){
    entry <- function.cache[[function.name]]
    file.name <- paste(fastr.test.folder, "/TestrGenBuiltin", function.name, ".java", sep="")
    writeLines(c(entry$code, "}"), file.name)
  }
}

#' @title Function that tags failing test cases for FastR as ignored
#' 
#' This function tags test cases with Ignore preporcessor. It requires a file with results of running tests
#' @param fastr.test.root folder with FastR test cases
#' @param result.file file with result of running FastR test cases
#' @export
#'
FastrTagIgnored <- function(fastr.test.root, result.file) {
  lines <- readLines(result.file)
  tests <- vector()
  for (line in lines)
    if (grepl("Micro-test failure", line))
      tests <- c(tests, gsub("Micro-test failure: (.*)\\((.*)\\)", "\\1", line))
  
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  files <- GetAllFiles(fastr.test.root, pattern = "*.java", TRUE)
  for (tc.file in files){
    lines <- readLines(tc.file)
    sink(tc.file)
    for (i in 1:length(lines)){
      line <- lines[i]
      cat(line, "\n", sep="")
      if (i < length(lines) && grepl("@Test", line)){
        tc.name <- gsub("public void (.*)\\((.*)", "\\1", trim(lines[i + 1]))
        if (tc.name %in% tests)
          cat("    @Ignore\n")
      }
    }
    cat("\n")
    sink()
  }
}
