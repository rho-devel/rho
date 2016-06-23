% File src/library/compiler/man/compile.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011 R Core Team
% Distributed under GPL 2 or later

\name{compile}
\alias{compile}
\alias{cmpfun}
\alias{cmpfile}
\alias{loadcmp}
\alias{disassemble}
\alias{enableJIT}
\alias{compilePKGS}
\alias{getCompilerOption}
\alias{setCompilerOptions}
\title{Byte Code Compiler}
\usage{
cmpfun(f, options = NULL)
compile(e, env = .GlobalEnv, options = NULL)
cmpfile(infile, outfile, ascii = FALSE, env = .GlobalEnv,
        verbose = FALSE, options = NULL)
loadcmp(file, envir = .GlobalEnv, chdir = FALSE)
disassemble(code)
enableJIT(level)
compilePKGS(enable)
getCompilerOption(name, options)
setCompilerOptions(...)
}
\arguments{
  \item{f}{a closure.}
  \item{options}{list of named compiler options}
  \item{env}{the top level environment for the compiling.}
  \item{file,infile,outfile}{pathnames; outfile defaults to infile with a .Rc
                             extension in place of any existing extension.}
  \item{ascii}{logical; should the compiled file be saved in ascii format?}
  \item{verbose}{logical; should the compiler show what is being compiled}
  \item{envir}{environment to evaluate loaded expressions in.}
  \item{chdir}{logical; change directory before evaluation?}
  \item{code}{byte code expression or compiled closure}
  \item{e}{expression to compile}
  \item{level}{integer; the JIT level to use}
  \item{enable}{logical; enable compiling packages if \code{TRUE}}
  \item{name}{character string; name of option to return}
  \item{...}{named compiler options to set}
}
\description{
  Stubs for API compatibility with GNU R.
}
\details{
  Rho doesn't use the GNU R byte compiler.  Stub versions of these
  functions are provided to prevent code that expects the byte compiler
  from breaking, but provide no speed advantage.
}
\keyword{programming}
