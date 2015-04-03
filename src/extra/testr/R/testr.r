# Copyright (c) 2013, Purdue University. All rights reserved.
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

# Generators -------------------------------------------------------------------------------------------------------------

#' @export
#' @title Checks that the given function has a proper signature for a generator content function. 
#' 
#' A generator content function takes the index of the value to be returned that must be named i, the generator itself named g, and the environment of the current test called env. The environment is a list containing values of all previously defined generators in the particular test instance. 
#' The function returns true if the given function has three arguments, named i, g and env. False otherwise.  
#' 
#' @param f function whose signature is to be checked.
#' @return TRUE if the function has the signature of a generator content function (arguments i, g and env) and FALSE otherwise
#' 
#' @seealso customGenerator
#' @examples
#' is.generatorContentFunction(function(i,g,env) { g$expr[[i]]} )
is.generatorContentFunction <- function(f) {
    identical(names(formals(f)), c("i", "g", "env"))
}

#' @export
#' @title Creates new custom generator.
#' 
#' A generator is identified by its content function which is called with the index of the value, the generator itself and already evaluated generators. The generator also must have assigned a name.
#' 
#'  Generators can be dependent or independent. Independent generators define how many tests a single test definition will be expanded to since for each possible permutation of independent generator values a new test is produced.
#' 
#' @param name Name of the generic - either string or an unquoted name
#' @param contentFunction Function that provides the content of the generator.
#' @param dependsOn Determines a generator on whose value this generator depends. That is if the generator the current one depends on changes, only then the dependent generator changes too.
#' @param length Number of values the generator provides, practically valid only for independent generators as dependent generators are simply evaluated as many times as the generator they depend on.
#' @param ... any other optional named arguments that will become fields of the returned generator S3 object. These additional arguments are not evaluated and are stored as their language objects.
#' @return The generator object that can be passed to the test function.
#' 
#' @seealso is.generatorContentFunction, test
customGenerator <- function(name, contentFunction, ..., dependsOn = NULL, length = 1) {
    if (! is.generatorContentFunction(contentFunction))
        stop("Generator content function must take exactly three arguments named i, g and env.")
    result <- eval(substitute(alist(...)))
    # check that only named arguments were supplied to the list
    n <- names(result)
    if (length(n) != length(result) || ("" %in% n))
        stop("Only named arguments can be supplied to the generator object apart from name, length and content function")
    # check that none of the arguments supplied corresponds with reserved values for the generators. Since name, length
    # contentFunction and dependsOn are all listed as arguments to this function clash can only happen for dependents
    if ("dependents" %in% n)
        stop("'dependents' is a reserved generator field and cannot be supplied explicitly")
    # get the name (convert its substitute to character if it is not yet)
    name <- substitute(name)
    if (typeof(name) == "character")
        result$name <- name
    else
        result$name <- as.character(name)
    # get the other arguments in the result object
    result$contentFunction <- contentFunction
    dependsOn <- substitute(dependsOn)
    if (! is.null(dependsOn)) {
        result$dependsOn <- as.character(dependsOn)
    } else {
        result$dependsOn <- NULL
        if (missing(length))
            stop("Length must be defined for an independent generator")
    }
    length <- as.integer(length)
    if (length < 1)
        stop("Generator must have length of at least one")
    result$length <- length
    class(result) <- "generator"
    result
}

#' @export
#' @title Shorthand for customGenerator function. 
#' 
#' @aliases customGenerator
#' @seealso customGenerator
#' @examples
#' cg("a", function(i, g, env) { 1 }, length = 5)
cg <- customGenerator


#' @export
#' @title Creates new generator that is defined by a list of its values.
#' 
#' This is a simplified version of the custom generator where instead of the content generator function, an actual list of the possible values for the generic is specified. 
#' 
#' @param name Name of the generator.
#' @param ... Values of the generator. The number of values determines the length of the generator. The values are not evaluated, but are stored as their language objects. 
#' @param dependsOn If specified, determines which other generator this one depends on.
#' @return The generator object that will have the length defined by number of its ... values and returns them one by one.
#' 
#' @seealso customGenerator
#' @examples
#' generator(a, 1, 2, 3, 4, 5)
#' generator(a, c(1,2), list(1,2))
generator <- function(name, ..., dependsOn = NULL) {
    ex <- eval(substitute(alist(...)))
    # convert name to character if required
    name <- substitute(name)
    dependsOn <- substitute(dependsOn)
    if (typeof(name) != "character")
        name <- as.character(name)
    if (!is.null(dependsOn))
        dependsOn <- as.character(dependsOn)
    # we must do the eval & substitute here so that the actual ASTs supplied to this function will be passed to the
    # underlying custom generator too
    eval(substitute(customGenerator(name, function(i,g,env) { g$expr[[i]] }, length = length(ex), dependsOn = dependsOn, expr = ex), list(ex = ex, name = name, dependsOn = dependsOn)))
}

#' @export
#' @title A shorthand for simple generator function
#' 
#' @seealso generator
#' @examples
#' g(a, 1, 2, 3)
g <- generator

#' @export 
#' @title Determines if given object is an instance of generator. 
#' 
#' A generator must inherit from a generator class. Likely this is produced by calling either customGenerator, or generator function. 
#' @param o Object to be tested.
#' @return TRUE if the argument is a generator, FALSE otherwise
#' @seealso customGenerator, generator
#' @examples
#' is.generator(1) # FALSE
#' is.generator(generator(a, 1, 2, 3)) # TRUE
is.generator <- function(o) {
    inherits(o, "generator")
}

#' @export
#' @title Determines if the given generator is dependent one.
#' 
#' @param g Generator to be tested.
#' @return TRUE if the generator is dependent, FALSE if independent.
#' @seealso customGenerator, generator
#' @examples
#' is.dependent(g(a, 1, 2, 3)) # FALSE
#' is.dependent(g(b, 1, 2, 3, dependsOn = b)) # TRUE
is.dependent.generator <- function(g) {
    !is.null(g$dependsOn)
}

#' @export
#' @title Determines if the given generator is independent, or not. 
#' 
#' @param g Generator to be tested.
#' @return TRUE if the generator is independent, FALSE if dependent.
#' @seealso customGenerator, generator
#' @examples
#' is.independent(g(a, 1, 2, 3)) # TRUE
#' is.independent(g(b, 1, 2, 3, dependsOn = b)) # FALSE
is.independent.generator <- function(g) {
    is.null(g$dependsOn)
}

#' @export
#' @title Generic method for S3 dependent and independent generators. Checks that the generator is dependent
#'
#' @param g Generator to be tested.
#' @return TRUE if the generator is dependent, FALSE if independent.
#' @seealso is.dependent.generator, customGenerator, generator
#' @examples
#' is.dependent(g(a, 1, 2, 3)) # FALSE
#' is.dependent(g(b, 1, 2, 3, dependsOn = b)) # TRUE
is.dependent <- function(o) {
    UseMethod("is.dependent", o)
}

#' @export
#' @title Generic method for S3 dependent and independent generators. Checks that the generator is independent
#'
#' @param g Generator to be tested.
#' @return TRUE if the generator is independent, FALSE if dependent.
#' @seealso is.independent.generator, customGenerator, generator
#' @examples
#' is.independent(g(a, 1, 2, 3)) # TRUE
#' is.independent(g(b, 1, 2, 3, dependsOn = b)) # FALSE
is.independent <- function(o) {
    UseMethod("is.independent", o)
}


#' @export
#' @title Returns the length of a generator. 
#' 
#' The length of a generator is number of unique values the generator can produce. This only has real meaning for independent generators as dependent generators can have length 1 which gets constantly reevaluated. 
#' 
#' @param g Generator whose length is to be determined. 
#' @return Length of the generator.
#' @seealso customGenerator, generator
#' @examples
#' length(g(a, 1, 2, 3)) # 3
#' length(g(b, 1, 2, dependsOn = a)) # 2
#' length(cg(c, function(i, g, env) { i }, dependsOn = a)) # 1
length.generator <- function(g) {
    g$length
}


# substitution ---------------------------------------------------------------------------------------------------------

#' @export
#' @title Substitutes given string or AST with the specified environment. 
#' 
#' @details
#' This function works in a way similar to the substitute function known in R with two main differences: 
#' 
#' 1) if the argument code is character, a string replacement is used instead of substitution and a character with replaced portions is returned. 
#' 2) substitution on AST objects also works for function calls and operators, not just variables.
#' 
#' Due to the above mentioned points, the correctness of the produced R code is not guaranteed. 
#' 
#' Variables and function calls are replaced according to the expected behavior. That is variable of the same name as a generator name is replaced by the value of that generator. Function with the same name as a generator is also replaced by the generator's value - this applies only to function name, not the function call itself. 
#' 
#' Operators can be replaced if they are written in a form of %A% where A is the name of the generator. It is assumed the generator will then list character identifiers fully replacing the operator. 
#' 
#' For character replacement, %A where A is name of the generator, is replaced by the generator value. %% will be replaced by %. 
#' 
#' @param code AST or string of the code that is to be replaced.
#' @param env list of the environment used for the replacement. 
#' @return Same type as the code, with replacements from env as described above. 
#' @seealso customGenerator, test
#' @examples
#' testSubstitute(a + b, list(a = 1)) # 1 + b
#' testSubstitute(1 %x% 2, list (x = "+")) # 1 + 2
#' testSubstitute(f(1, 2), list( f = "foo")) # foo(1, 2)
#' testSubstitute("%a + %b %c %%in%% container", list(a = 1, b = 2, c = 3)) # "1 + 2 3 %in% container" -- invalid R code

testSubstitute <- function(code, env) {
    c = substitute(code)
    if (typeof(c) == "character") {
        c <- gsub("%%",as.character(as.raw(0)), c)
        if (length(env) != 0) for (n in sort(names(env), decreasing = TRUE))
            if (typeof(env[[n]]) == "character")
                c <- gsub(paste("%", n, sep = ""), env[[n]], c)
        else
            c <- gsub(paste("%", n, sep = ""), deparse(env[[n]]), c)
        gsub(as.character(as.raw(0)), "%", c)
    } else {
        # duplicate the env values for the operators as well
        for (n in names(env)) 
            env[[paste("%", n, "%", sep = "")]] <- env[[n]]
        testSubstituteAST(c, env)
    }
}

# test substitute helper function
testSubstituteAST <- function(ast, env) {
    if (is.symbol(ast)) {
        aName <- as.character(ast)
        if (aName %in% names(env))
            env[[aName]]
        else
            ast
    } else if (is.call(ast)) {
        aName <- as.character(ast[[1]])
        if (aName %in% names(env))
            ast[[1]] <- as.name(env[[aName]])
        for (i in 2:length(ast))
            ast[[i]] <- testSubstituteAST(ast[[i]], env)
        ast
    } else if (is.pairlist(ast)) {
        # for pairlists do not substitute at all
        #for (i in 1: length(ast))
        #    ast[[i]] <- testSubstitute2(ast[[i]], env)
        ast
    } else if (is.double(ast) || is.logical(ast) || is.integer(ast) || is.complex(ast) || is.character(ast)) {
        ast # return the ast with no modification
    } else {
        stop("unknown type ", typeof(ast))
    }
}

# test generator -------------------------------------------------------------------------------------------------------

#' @export
#' @title Function that creates a test based on its code and checks. 
#' 
#' The function identifies new test, specifies its generators and their values and the checks used to verify its correctness. The test can optionally have a name specified.
#' 
#' The test is expanded to as many tests as there are permutations of its independent generators. The generator values are incremented in order of their definition so that the last defined generator is the LSB value. For example a test with three generators will be expanded accordingly:
#' 
#' test(
#'   g(a, 1, 2),
#'   g(b, 3, 4),
#'   g(c, 5, 6),
#'   a + b + c
#' )
#' 
#' a b c
#' 1 3 5
#' 1 3 6
#' 1 4 5
#' 1 4 6
#' 2 3 5
#' 2 3 6
#' 2 4 5
#' 2 4 6
#' 
#' A test may also have generators that depend on others. These do not increase the number of expanded tests but allow a mechanism of linking two or more generated values together.
#' 
#' Any usage of the generator name in either the code, or the output test will be replaced by the respective value of that generator for the particular test. 
#' 
#' To expand the tests, makeTests function must be called. 
#' 
#' @param ... generators, code of the test. Code must be the last argument. Code can be eitcher character, or an AST. Other named arguments will become fields of the test object (but this has no effect on the standard test implementation)
#' @param name optional name of the test. 
#' @param o Expected output of the test. Can contain generators. If not specified, and no error is expected, the output will be calculated automatically by executing the test. If the output contains generators, these will be substitued with their values. If the output is character string, it will still be substitued, but not deparsed. Only the last value of the test can be checked. A code can be supplied instead of a value in which case the code will be exectuted to produce the correct output (this will happen in the test expansion, not on target). 
#' @param w Character of warning messages to be expected. The messages can be either parts of the message or full lines. If there are multiple warning messages all must be found within the test result. 
#' @param e Character of error messages expected. The messages can be either parts of the message or full lines. If there are multiple error messages all must be found within the test result. 
#' @return NULL - the tests are added automatically to the makeTests. 
#' @seealso makeTests, cg, g, testSubstitute

test1 <- function(..., name = NULL, o = NULL, w = NULL, e = NULL) {
    # convert name to character if required, preserve name null if unnamed
    if (typeof(name) != "character")
        name <- as.character(substitute(name))
    if (length(name) == 0)
        name <- NULL
    # get the arguments in dots and separate them as code commands and code (code being the last one)
    commands <- eval(substitute(alist(...)))
    if (!missing(e))
        e <- substitute(e)
    if (!missing(w))
        w <- substitute(w)
    if (length(commands) == 0)
        stop("The test must have at least a code specified.")
    code <- commands[[length(commands)]]
    commands <- commands[-length(commands)]
    # now that we have the commands, make them into generators, checks and conditions
    separatedCommands <- separateCommands(commands)
    # now ennumerate the tests 
    if (missing(o))
        enumerateTests(name, code, w, e, separatedCommands)
    else
        enumerateTests(name, code, w, e, separatedCommands, o = substitute(o))
}

separateCommands <- function(commands) {
    result <- list(
        independentGenerators = list(),
        dependentGenerators = list(),
        test = list()
        )
    cmdNames = names(commands)
    if (length(commands) != 0) {
        for (i in 1:length(commands)) {
            # first check if it is a named argument, in which case its AST will be stored in the test itself
            if (!identical(cmdNames[[i]],"") && !is.null(cmdNames[[i]])) {
                if (cmdNames[[i]] %in% c("name",
                                         "env",
                                         "code",
                                         "originalCode",
                                         "e",
                                         "w",
                                         "o",
                                         "independentGenerators",
                                         "dependentGenerators",
                                         "generatorValues"
                                         ))
                    stop("Cannot use reserved name ",cmdNames[[i]]," in test field creation")
                test[[cmdNames[[i]]]] <- commands[[i]]
            } else {
                cmd <- eval(commands[[i]])
                if (is.generator(cmd)) {
                    if (cmd$name %in% c(names(result$independentGenerators), names(result$dependentGenerators)))
                        stop("Generator ", cmd$name, " already defined for the test.")
                    if (is.independent(cmd)) { # for independent generators, only store them
                        result$independentGenerators[[cmd$name]] <- cmd
                    } else { # for dependent generators update the master generator's dependents list
                        result$dependentGenerators[[cmd$name]] <- cmd
                        if (! cmd$dependsOn %in% names(result$independentGenerators))
                            stop("Generator ", cmd$name, " depends on unknown generator ", cmd$dependsOn)
                        # add the dependent generator to the master list and replace it
                        ig <- result$independentGenerators[[cmd$dependsOn]]
                        ig$dependents <- c(ig$dependents, cmd$name)
                        result$independentGenerators[[ig$name]] <- ig
                    }
                } else {
                    stop("Only generator, or a named argument can be passed as an argument to a test")
                }
            }
        }
    }
    result
}

enumerateTests <- function(name, code, w, e, separatedCommands,  o = NULL) {
    increaseGenerator <- function(i = length(ig)) {
        if (i != 0) {
            g <- ig[[i]]
            # first increase dependent generators
            for (dgn in g$dependents) {
                dg <- dg[[dgn]] 
                if (dPos[[dg$name]] == dMax[[dg$name]])
                    dPos[[dg$name]] <<- 1
                else
                    dPos[[dg$name]] <<- dPos[[dg$name]] + 1
            }
            # now increase the generator itself and perform recursive increase ifoverflow
            if (iPos[[i]] == iMax[[i]]) {
                iPos[[i]] <<- 1
                if (i > 1)
                    increaseGenerator(i-1)
            } else {
                iPos[[i]] <<- iPos[[i]] + 1
            }
        }
    }
    # shorthands
    ig <- separatedCommands$independentGenerators
    dg <- separatedCommands$dependentGenerators
    # determine the number of tests to be produced and max lengths for the independent generators
    iMax <- sapply(seq_len(length(ig)), function(i) { length(ig[[i]]) })
    names(iMax) <- names(ig)
    if (length(iMax) == 0)
        n <- 1
    else 
        n <- prod(iMax)
    # determine the max value for dependent generators
    dMax <- sapply(seq_len(length(dg)), function(i) { length(dg[[i]]) })
    names(dMax) <- names(dg)
    # generate generator values vectors
    iPos <- rep(1, length(ig))
    names(iPos) <- names(ig)
    dPos <- rep(1, length(dg))
    names(dPos) <- names(dg)
    for (t in 1:n) {
        # first determine the values of the generators and store them to the environment list
        env <- list()
        for (g in ig)
            env[[g$name]] <- g$contentFunction(iPos[[g$name]], g, env)
        for (g in dg) {
            env[[g$name]] <- g$contentFunction(dPos[[g$name]], g, env)
        }
        # now we have the generator values in place, get the substitued code
        codeStr <- eval(substitute(testSubstitute(code, env), list(code = code)))
        if (typeof(codeStr) != "character") {
            codeStr <- deparse(codeStr)
        }
        # create the test
        if (is.null(name)) {
            tname <- length(tests) + 1
        } else {
            tname <- name
            if (length(env) != 0) {
                s <- NULL
                for (n in names(env))
                    s <- c(s, paste(n, "=", deparse(env[[n]])))
                s <- paste(s, collapse = ", ")
                tname <- paste(tname, " [", s, "]", sep = "")
            }
        }
        test <- c(
            list(
                # if changing these, also change the check for reserved names in separateCommands function above
                name = tname,
                env = env, 
                code = codeStr, 
                originalCode = code, 
                independentGenerators = separatedCommands$independentGenerators,
                dependentGenerators = separatedCommands$dependentGenerators,
                generatorValues = c(iPos, dPos)
                ),
            separatedCommands$test
        )
        if (!missing(o))
            test$o <- eval(eval(substitute(testSubstitute(o,env), list(o = o))))
        if (!is.null(w))
            test$w <- w
        if (!is.null(e)) 
            test$e <- e
        class(test) <- "testInstance"
        tests <<- c(tests, list(test))
        # increase the generators
        increaseGenerator()
    }
}

#' @export 
#' @title S3 method to expand a test to a string that can be stored to the expanded tests file. 
#' 
#' Such a method must return the string corresponding to that test definition. May be overriden for different tests. 
#' @param t Test to be expanded
#' @param testId Id of the test within the suite. 
#' @seealso test
expandTest <- function(t, ...) {
    UseMethod("expandTest", t)
}

# expansion of the default test
expandTest.testInstance <- function(test, testId) {
    code <- test$code
    callArgs <- NULL
    if ("o" %in% names(test)) {
        callArgs <- c(callArgs, paste("o = ", test$o))
        if ("e" %in% names(test)) 
            stop("Output and error cannot be defined at the same time")
    } else if ("e" %in% names(test)) {
        callArgs <- c(callArgs, paste("e = ", deparse(test$e)))
    } else {
        cat("Test", test$name, "does not have output specified, calculating...\n")
        o <- eval(eval(substitute(testSubstitute(code, test$env), list(code = test$originalCode))), envir = new.env(parent=baseenv()))
        callArgs <- c(callArgs, paste("o = ",deparse(o), sep = ""))
    }
    if ("w" %in% names(test))
        callArgs <- c(callArgs, paste("w = ", deparse(test$w)))
    result <- paste(code, collapse = "\n  ")
    if (! is.null(test$name))
        result <- c(result, paste("  name = \"",gsub('"',"\\\\\"",gsub("\\\\","\\\\\\",test$name)),"\"", sep = ""))
    for (arg in callArgs) { 
        result <- c(result, paste("  ",arg, sep = ""))
    }
    result <- paste(result, collapse = ",\n")
    paste("test(id=", testId, ",\n  ", result,"\n)\n\n", sep ="")
}

#' @export
#' @title Prettyprint for an expanded test instance. 
#' 
#' Prints the test name, generators and code with generators replaced. 
#' 
#' @param t Test to be printed. 
#' @seealso test

print.testInstance <- function(t) {
    cat("Test", t$name, "\n")
    cat("Generators:\n")
    print(t$generatorValues)
    cat("Code:\n", t$code, "\n")
}

#' @export
#' @title Launches the test compiler that takes the unexpanded tests with generators and converts them to their expanded variants. 
#' 
#' Recursively walks all tr (.tr and .TR) files on given root. For each file reads its tests, expands them and stores them to the same file but under the destRoot folder. 
#' 
#' It is assumed that the tests are defined using the test function, but any object that inherits from "testInstance" and overrides its own expand method will work properly. 
#' 
#' The recursive folders (if any) in the dest folder will be created automatically. 
#' 
#' @param root Folder from where to look for R files with unexpanded tests
#' @param destRoot Folder to which the expanded tests will be stored
#' @param showCode if TRUE, code of the expanded tests will also be printed to stdout
#' @return NULL
#' @seealso test
#' @examples
#' makeTests("c:/unexpandedTests", "c:/expandedTests", showCode <- TRUE)

makeTests <- function(root, destRoot, showCode = FALSE) {
    total <- 0
    nFiles <- 0
    i = 1
    for (f in list.files(root, pattern=".[tT][rR]$", recursive = TRUE)) {
        nFiles <- nFiles + 1
        filename <- paste(root,"/", f, sep = "")
        cat(filename,"...\n")
        tests <<- list()
        cat("Analyzing file", filename, "\n")
        source(filename, local = FALSE)
        total <- total + length(tests)
        outFilename <- gsub(".[tT][rR]$", ".r", gsub(root, destRoot, filename))
        cat("  Writing", length(tests), "tests to file", outFilename,"...\n")
        dir.create(dirname(outFilename), recursive = TRUE, showWarnings = FALSE)
        f <- file(outFilename, "wt")
        for (t in tests) {
            code <- expandTest(t, i)
            i <- i + 1
            cat(file = f, code)
            if (showCode)
                cat(code)
        }
        close(f)   
    }
    rm(tests, envir = globalenv())
    cat("Total", total, "tests created out of", nFiles, "files.\n")
} 


