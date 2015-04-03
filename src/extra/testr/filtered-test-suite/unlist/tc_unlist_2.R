expected <- eval(parse(text="c(13823, NA)"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(c(13823, NA)), TRUE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

