expected <- eval(parse(text="c(\"  9 \", \" 13 \", \" 13+\", \" 18 \", \" 23 \", \" 28+\", \" 31 \", \" 34 \", \" 45+\", \" 48 \")"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(\"  9 \", \" 13 \", \" 13+\", \" 18 \", \" 23 \", \" 28+\", \" 31 \", \" 34 \", \" 45+\", \" 48 \"), TRUE, NULL, 0L, NULL, 0L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

