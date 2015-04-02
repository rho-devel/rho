expected <- eval(parse(text="c(\"a\", \"b\", \"c\", \"NA\", \"d\", NA)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(\"a\", \"b\", \"c\", \"c\", \"b\", \"a\", \"NA\", \"d\", \"d\", NA), FALSE, FALSE, NA)"));      
.Internal(`unique`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

