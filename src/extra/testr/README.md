testR - test case generation for R
=====

TestR implementation in R. It provides a framework for unit tests generation from source code and for test execution, and filtering of test cases based on C code coverage using `gcov` and R code coverage using `rcov` (https://github.com/RomanTsegelskyi/rcov).

This is the testR-py reimplementation and extension in R language. For the older testR version written in Python 3, please see the renamed testr-py repo
on github: https://github.com/allr/testr-py

[![Travis-CI Build Status](https://travis-ci.org/allr/testr.png?branch=master)](https://travis-ci.org/allr/testr)
[![Coverage Status](https://coveralls.io/repos/allr/testr/badge.svg?branch=master)](https://coveralls.io/r/allr/testr?branch=master)

# Installation
Development of package just started recently and is far away from CRAN release yet.

It can be installed easily with the nifty function of the devtools package from CRAN:

```r
    library(devtools)
    install_github('allr/testr')
```
Or download the sources and build manually. If you're running R on Windows, you need to install Rtools.

Automatic Test Case Generation
==============================

TestR includes tools to capture function calls in R programs and automatically convert them into test cases. 
Unevaluated promises needs to be handled specially, which will be dealt with in future work.

It is possible to generate test and filter test cases with calling a single function from the `testr` package:
```r
```
Which will capture function calls from source code, generate test cases and filter them based on C and R code coverage. Note that this call needs a R VM compiled with `gcov` support as explained in this [section](). For more details on the single steps, please look into further sections.

Generate Capture File
---------------------

The usage of different functions in the R programs are traced and recorded in a text file named `capture`. To decorate function for capturing output call:

```r
DecorateSubst(<FuncName>|<FuncObject>)
```

The capture file consists of entries, each of which is a record of a function call made in the execution with the following fields and the 
function calls will be replayed in test cases if valid.

  - func: function name
  - args: list of deparsed argument objects
  - retn: deparsed returned objects

For example,
```r
DecorateSubst(agrep)
example(agrep)
```

Will generate tracing information for call to `agrep` in folder `capture` in current working directory.

Generate Test Cases
-------------------

TestGen converts a given capture file into test case set. To do so, one needs to call `TestGen`:

```r
TestGen(<PATH_TO_CAPTURE_FILE>, <OUTPUT_DIR>)
```

Each run of TestGen will create a folder under `<OUTPUT_DIR>` named with the current date and time to store the test case set generated.
The set consists of files with name `tc_<FUNC_NAME>.r`, where each function has its tests put together in a single file. A file named 
`bad_arguments` will be placed beside the test files for logging the invalid entries of the capture file. An entry is invalid if the 
recorded argument and/or return values/objects cannot be restored properly due to a variety of reasons. A typical senario is when a 
function takes environment as arguments. For details, see comments in testgen.r. Under `<OUTPUT_DIR>`, a link symbol `last` is set to 
point to the latest generated test set folder. The overall directory structure of the test set would look like:

    <OUTPUT_DIR>/
        last
        2013-10-17 13:42:43/
            bad_arguments
            tc_foo.r
            tc_bar.r
            ...


Run Test Cases
--------------

The test cases are generated in the compatible format with the test harness of TestR. To run the test set under `<TC_DIR>`, call the
following function in `target.r`:

```r
RunTests(<TC_DIR>)
```

Filtering generated test cases
=========================

One way to assess the completeness of the test set is to measure the code coverage rate. TestR includes a coverage reporter which 
supports generating various forms of summary of C file coverage by processing the output of `gcov`. Moreover `testR` uses [rcov](https://github.com/RomanTsegelskyi/rcov) for reporting R code coverage. This section briefly explains
how to use the reporter and filtering of test cases based on coverage.

Instrument GNU-R with GCOV
--------------------------

To use GNU-R as the tested VM, one needs to first build it with `gcov` support. Following shows how to properly set up the variables 
during configuration:

    ./configure CFLAGS='-O0 -fprofile-arcs -ftest-coverage' LDFLAGS='-fprofile-arcs' 

After the build is done, for every .o file, a corresponding .gcno file will be generated for storing the source code structural 
information. Any C file not accompanied by a .gcno file was excluded in the compilation, and therefore will be excluded as well in 
the coverage report. `gcov` stores the coverage information in .gcda files and the coverage reporter relies on them to compute the
related numbers, therefore you may want to verify the existence of .gcda files before calling the reporter. For details about `gcov`, 
look at http://gcc.gnu.org/onlinedocs/gcc/index.html#toc_Gcov.


Measure Code C Coverage
---------------------

To invoke the reporter, call the following function located in coverage.r. The required argument is the the top level directory that
contains the .gcda files. For details of the result report, see comments in coverarge.r.

    MeasureCoverage(<PATH_TO_GCDA_FILES>)

Filtering generated test cases
----------------

To filter generated test cases in a single file call

```r
processTC(<TC_FILE>, <OUTPUT_DIR>, <R_HOME>, <SRC_DIR>)
```

Note that all test cases will be split into single files.

Where,
* `<TC_FILE>` is a generated file with test cases
* `<OUTPUT_DIR>` is a folder with results, 
* `<R_HOME>` is a R VM compiled with `gcov` support
* `<SRC_DIR>` is a folder in R VM to measure coverage (by default `src/main')
