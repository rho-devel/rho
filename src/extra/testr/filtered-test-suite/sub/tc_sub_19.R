expected <- eval(parse(text="character(0)"));               
test(id=0, code={               
argv <- eval(parse(text="list(\"([^:]*):(.*)\", \"\\\\2\", character(0), FALSE, FALSE, FALSE, FALSE)"));               
.Internal(sub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));               
}, o=expected);               

