expected <- eval(parse(text="\"checkRd: (-3) evalSource.Rd:157: Unnecessary braces at ‘{\\\"sourceEnvironment\\\"}’\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"checkRd: (%d) %s\", -3, \"evalSource.Rd:157: Unnecessary braces at ‘{\\\"sourceEnvironment\\\"}’\")"));           
.Internal(sprintf(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

