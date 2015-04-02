expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE)"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(TRUE, TRUE, TRUE, TRUE), FALSE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

