expected <- eval(parse(text="structure(c(\"NA\", \" 1\", \" 1\", \" 1\"), .Names = c(\"<none>\", \"- x4\", \"- x2\", \"- x1\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(NA, 1, 1, 1), .Names = c(\"<none>\", \"- x4\", \"- x2\", \"- x1\")), FALSE, 5L, 0L, NULL, 3L, TRUE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

