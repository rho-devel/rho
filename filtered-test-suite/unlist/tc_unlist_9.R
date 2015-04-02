expected <- eval(parse(text="c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, NA)"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(c(TRUE, FALSE, FALSE, FALSE, FALSE), c(TRUE, TRUE, TRUE, TRUE, NA)), FALSE, FALSE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

