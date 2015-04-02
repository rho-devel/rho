expected <- eval(parse(text="c(NA, NA, NA, NA, NA, \"Ripley\", \"Venables & Smith\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(NA, NA, NA, NA, NA, \"Ripley\", \"Venables & Smith\"), FALSE, NULL, 0L, NULL, 3L, FALSE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

