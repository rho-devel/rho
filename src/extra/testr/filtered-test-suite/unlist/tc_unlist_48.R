expected <- eval(parse(text="c(3L, 0L, 0L)"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(c(3L, 0L, 0L)), class = \"numeric_version\"), TRUE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

