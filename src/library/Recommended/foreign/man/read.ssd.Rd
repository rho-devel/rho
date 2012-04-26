% This file is part of the 'foreign' package for R
% It is distributed under the GPL version 2 or later

% Original copyright (c) 2002 VJ Carey
% Changes copyright (c) 2002-7 R Development Core Team

\name{read.ssd}
\alias{read.ssd}
\title{Obtain a Data Frame from a SAS Permanent Dataset, via read.xport }
\description{
  Generates a SAS program to convert the ssd contents to SAS transport format
  and then uses \code{read.xport} to obtain a data frame.
}
\usage{
read.ssd(libname, sectionnames, 
   tmpXport=tempfile(), tmpProgLoc=tempfile(), sascmd="sas")
}
\arguments{
  \item{libname}{character string defining the SAS library (usually a
    directory reference)}
  \item{sectionnames}{character vector giving member names. These are
    files in the \code{libname} directory. They will usually have a
    \code{.ssd0x} or \code{.sas7bdat} extension, which should be omitted.}
  \item{tmpXport}{character  string: location where temporary xport
    format archive should reside -- defaults to a randomly named file
    in the session temporary directory, which will be removed.}
  \item{tmpProgLoc}{character  string: location where temporary
    conversion SAS program should reside -- defaults to a randomly named
    file in session temporary directory, which will be removed on
    successful operation.}
  \item{sascmd}{character string giving full path to SAS executable.}
}
\details{
  Creates a SAS program and runs it.

  Error handling is primitive.
}
\value{
  A data frame if all goes well, or \code{NULL} with warnings and some
  enduring side effects (log file for auditing)
}
\author{
  For Unix: VJ Carey \email{stvjc@channing.harvard.edu}
}
\note{
  \strong{This requires SAS to be available.}  If you have a SAS dataset
  without access to SAS you will need another product to convert it to a 
  format such as \code{.csv}, for example \sQuote{Stat/Transfer} or 
  \sQuote{DBMS/Copy} or the \sQuote{SAS System Viewer} (Windows only).
}
\seealso{\code{\link{read.xport}}}
\examples{
## if there were some files on the web we could get a real
## runnable example
\dontrun{
R> list.files("trialdata")
 [1] "baseline.sas7bdat" "form11.sas7bdat"   "form12.sas7bdat"  
 [4] "form13.sas7bdat"   "form22.sas7bdat"   "form23.sas7bdat"  
 [7] "form3.sas7bdat"    "form4.sas7bdat"    "form48.sas7bdat"  
[10] "form50.sas7bdat"   "form51.sas7bdat"   "form71.sas7bdat"  
[13] "form72.sas7bdat"   "form8.sas7bdat"    "form9.sas7bdat"   
[16] "form90.sas7bdat"   "form91.sas7bdat"  
R> baseline <- read.ssd("trialdata","baseline")
R> form90 <- read.ssd("trialdata","form90")

## Or for a Windows example
sashome <- "/Program Files/SAS/SAS 9.1"
read.ssd(file.path(sashome, "core", "sashelp"), "retail",
         sascmd = file.path(sashome, "sas.exe"))
}
}
\keyword{file}
