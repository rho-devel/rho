expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"Error in as.POSIXlt.character(x, tz, ...) : \\n  character string is not in a standard unambiguous format\\n\")"));      
.Internal(seterrmessage(argv[[1]]));      
}, o=expected);      

