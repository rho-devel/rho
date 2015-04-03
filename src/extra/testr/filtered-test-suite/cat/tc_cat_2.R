expected <- eval(parse(text="NULL"));      
test(id=0, code={      
argv <- eval(parse(text="list(list(\"Loading required package: splines\\n\"), structure(2L, class = c(\"terminal\", \"connection\")), \"\", FALSE, NULL, FALSE)"));      
.Internal(`cat`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));      
}, o=expected);      

