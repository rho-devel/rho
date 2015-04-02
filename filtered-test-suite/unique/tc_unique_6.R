expected <- eval(parse(text="c(\"colors\", \"colours\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(\"colors\", \"colours\"), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

