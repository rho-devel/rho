expected <- eval(parse(text="\"\\\"Version of 'graph' is too old --- no tests done here!\\\\n\\\"\""));          
test(id=0, code={          
argv <- eval(parse(text="list(\"Version of 'graph' is too old --- no tests done here!\\n\", 60L, FALSE, 69, -1L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

