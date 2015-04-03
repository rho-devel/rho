expected <- eval(parse(text="c(0L, 0L, 0L, 0L, 0L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(\"May\", \"Jun\", \"Jul\", \"Aug\", \"Sep\"), c(NA, NaN), 0L, NULL)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

