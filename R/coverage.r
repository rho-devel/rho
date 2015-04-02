# DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
#
# This code is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 only, as
# published by the Free Software Foundation.
#
# This code is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# version 2 for more details (a copy is included in the LICENSE file that
# accompanied this code).
#
# You should have received a copy of the GNU General Public License version
# 2 along with this work; if not, write to the Free Software Foundation,
# Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
#
# Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
# or visit www.oracle.com if you need additional information or have any
# questions.

#' @export
#' @title Coverage Report on Specified R Virtual Machine Source Code
#'
#' @description This function works with the GNU coverage tool, gcov, to report code coverage of the
#'  tested R virtual machine. and The VM must have been compiled with gcov support and executed at least
#'  once before this function is executed for meanful statistics. Note that, gcov is designed such
#'  that the coverage results are accumulated, so for a fresh new result of a test suit, call
#'  \code{\link{reset}} before executing the test suite. The coverage granularition spans line, function,
#'  and file. 
#'
#' @param root a directory or a single C file that contains or is instrumented VM source.
#' @param verbose wheather to display resulting info
#' @param exclude.header whether to include header files in statistics.
#' @param file.detail whether to display statistic details for files. Only works if verbose is true.
#' @param func.detail whether to display statistic details for functions. Only works if verbose is true.
#' @param file.keyword filtering the result by only including files whose names contain the keyword.
#' @param func.keyword filtering the result by only including functions whose names contain the keyword.
#' @param ignore.case whether the keywords should be case-sensitive.
#' @return list(file=file.df, func=func.df)
#'
#' @details In this section we give some design thoughts then in example section some sample
#'  reports and the interpretation.
#' 
#'  As a testcase contributor, the right way to use the coverage rate is to look for increments
#'  after new testcases are added rather than aiming for 100% coverage. The reason is that, due
#'  to the file inclusion feature of C language, exhaustive tests will be hardly shown as 100%
#'  coverage (could be lower or higher). Specifically, if one piece of code in a header file gets
#'  included in two C files, gcov reports it twice and C in C inclusion will make the situation
#'  even more complicated. So we give users the choices to either include both (over estimation)
#'  or include none (under estimation) in the overall result by allowing them to choose whether
#'  to include the header files in the statistics. Identifying duplication is almost impossible.
#'
#'  On the other hand, gcov produces precise result object-wise. The term "object" is used as is
#'  in "object file" during linking, referring to a group of files glued together by inclusion,
#'  usually a C file and several header files but in rare case several C files and several header
#'  files. The coverage within the range of object is precise as C compilers guarantee no duplication
#'  by nature. In that, the object information is included in the file/function detail tables for
#'  users' reference.
#'
#'@examples
#'\dontrun{
#'MeasureCoverage()
#'}

MeasureCoverage <- function(root, verbose = TRUE, exclude.header=TRUE, file.detail=FALSE, func.detail=FALSE, file.keyword="", func.keyword="", ignore.case=TRUE) {
  if (missing(root)) 
    stop("A directory containing VM source files must be specified!");
  if (.Platform$OS.type=="windows") {
    stop("Not supported on Windows!");
  }
  if (length(grep("[.]c$", root, ignore.case=TRUE))) { 
    cfiles <- root 
  } else { 
    if (!length(list.files(path=root, recursive=TRUE, pattern=".gcda$")))
      stop("No coverage information in the specified folder! .gcda files do not exist")
    cfiles <- list.files(path=root, recursive=TRUE, pattern=".c$")
  }
  if (!length(cfiles))
    stop("Root param has no files with .c extension")

  gcovOnSingleFile <- function(f) {
	  path <- file.path(root, dirname(f), fsep=.Platform$file.sep);
	  file <- file.path(root, f,          fsep=.Platform$file.sep);
          cmd <- paste("gcov", "-p", "-n", "-f",  "-o", path, file, sep=" ");
	  r <- system(cmd, intern=TRUE, ignore.stderr=TRUE);
	  
	  # for gcov4.7 output
	  i <- which(r == "No executable lines");
	  j <- i - 1;
	  i <- append(i,j);
	  if (length(i) != 0)
		  r <- r[-i];
	  
	  fileLines <- r[grep("^File", r)];
	  funcLines <- r[grep("^Function", r)];
	  dataLines <- r[grep("^Lines executed", r)];
	  if (length(fileLines) != 0) {
		  files.len <- length(fileLines);# - length(noExecFiles);
		  funcs.len <- length(funcLines);
		  data.len  <- length(dataLines);
		  if (files.len + funcs.len != data.len) stop("Unexpected gcov output format!");
		  files <- matrix(unlist(strsplit(fileLines,"'")), ncol=2, byrow=TRUE)[,2];
		  funcs <- matrix(unlist(strsplit(funcLines,"'")), ncol=2, byrow=TRUE)[,2];
		  data  <- strsplit(unlist(strsplit(dataLines, "Lines executed:")), "% of ")[seq(2,2*data.len,2)];
		  func.data <- data[1:funcs.len];
		  file.data <- data[(funcs.len+1):data.len];
		  func.df <- data.frame(cbind(funcs, matrix(unlist(func.data), ncol=2, byrow=TRUE)), stringsAsFactors=FALSE);
		  file.df <- data.frame(cbind(files, matrix(unlist(file.data), ncol=2, byrow=TRUE)), stringsAsFactors=FALSE);
		  colnames(func.df) <- c("Func", "CovLn%", "LOC");
		  colnames(file.df) <- c("File", "CovLn%", "LOC");
		  calCovLnAndAddObj <- function(df) {
			  covLn <- round(as.numeric(df$'CovLn%') / 100.0 * as.numeric(df$'LOC'), digits=0);
			  df <- cbind(df, 'CovLn'=covLn);
			  cbind(df, Obj=f);        
		  }
		  func.df <- calCovLnAndAddObj(func.df)[,c(5,1,4,3,2)]; # Obj Func CovLn LOC CovLn%
		  file.df <- calCovLnAndAddObj(file.df)[,c(5,1,4,3,2)]; # Obj File CovLn LOC CovLn%
		  list(file=file.df, func=func.df);
	  } else { NULL }
  }
  
  result <- Map(gcovOnSingleFile, cfiles);
  file.df <- data.frame('File'=vector(),'CovLn%'=vector(),'LOC'=vector(),'CovLn'=vector(),'Obj'=vector());
  func.df <- data.frame('Func'=vector(),'CovLn%'=vector(),'LOC'=vector(),'CovLn'=vector(),'Obj'=vector());
  for (item in result) {
    file.df <- rbind(file.df, item$file);
    func.df <- rbind(func.df, item$func);
  }
  file.df <- file.df[grep(file.keyword, file.df$'Obj',  ignore.case=ignore.case),];
  func.df <- func.df[grep(file.keyword, func.df$'Obj',  ignore.case=ignore.case),];
  func.df <- func.df[grep(func.keyword, func.df$'Func', ignore.case=ignore.case),];
  if (exclude.header) {
    file.df <- file.df[grep("[.]c$", file.df$'File', ignore.case=ignore.case),];  
  }
  
  # line file coverage
  totalLine.file        <- sum(as.numeric(file.df$'LOC'));
  totalCovLine.file     <- sum(as.numeric(file.df$'CovLn'));
  totalCovLinePcnt.file <- round(totalCovLine.file / totalLine.file * 100, digits=10);
  # line func coverage
  totalLine.func        <- sum(as.numeric(func.df$'LOC'));
  totalCovLine.func     <- sum(as.numeric(func.df$'CovLn'));
  totalCovLinePcnt.func <- round(totalCovLine.func / totalLine.func * 100, digits=10);
  # func coverage
  totalFunc        <- nrow(func.df);
  totalCovFunc     <- nrow(func.df[as.numeric(func.df$'CovLn')>0,]);
  totalCovFuncPcnt <- round(totalCovFunc / totalFunc * 100, digits=2);
  # file coverage
  totalFile        <- nrow(file.df);
  totalCovFile     <- nrow(file.df[as.numeric(file.df$'CovLn')>0,]);
  totalCovFilePcnt <- round(totalCovFile / totalFile * 100, digits=2);

  if (verbose){
    cat("========-------- Coverage Report --------========\n");
    cat("\n");
    cat(">>> Configration:\n");
    cat("\n");
    cat("- src root:         ", root,           "\n", sep="");
    cat("- file keyword:     ", file.keyword,   "\n", sep="");
    cat("- func keyword:     ", func.keyword,   "\n", sep="");
    cat("- igore case:       ", ignore.case,    "\n", sep="");
    cat("- exclude header:   ", exclude.header, "\n", sep="");
    cat("\n");
    cat(">>> Coverage:\n");
    cat("\n");
    cat("* Line (file): ", totalCovLine.file, " out of ", totalLine.file, " (", totalCovLinePcnt.file, "%)\n", sep="");
    cat("* Line (func): ", totalCovLine.func, " out of ", totalLine.func, " (", totalCovLinePcnt.func, "%)\n", sep="");
    cat("* File:        ", totalCovFile,      " out of ", totalFile,      " (", totalCovFilePcnt,      "%)\n", sep="");
    cat("* Func:        ", totalCovFunc,      " out of ", totalFunc,      " (", totalCovFuncPcnt,      "%)\n", sep="");
    if (file.detail) {
      cat("\n");
      cat("----------------   File Detail   ----------------\n");
      cat("\n");
      print(file.df, row.names=FALSE);
    }
    if (func.detail) {
      cat("\n");
      cat("----------------   Func Detail   ----------------\n");
      cat("\n");
      print(func.df, row.names=FALSE);
    }
    cat("\n");
    cat("=================================================\n");
  }
  return (list(file=file.df, func=func.df));
}

#' @export
#' @title Reset coverage information
#'
#' @description This function deletes gcov information files (*.gcda) thus clearing any previously collected coverage information is erased.
#' @param root a directory or a single C file that contains or is instrumented VM source.
#'
ResetCoverageInfo <- function(root) {
  cat("reset called\n")
  if (missing(root)) stop("A directory containing VM source files must be specified!");
  if (.Platform$OS.type=="unix") {
    cmd <- paste("find", root, "-name", "\'*.gcda\'", "-delete", sep=" ");
    system(cmd, ignore.stdout=TRUE, ignore.stderr=TRUE);
  } else if (.Platform$OS.type=="windows") {
    stop("Not supported on Windows!");
  } else {
   stop("Unknown operating system type: ", .Platform$OS.type);
  }
}


