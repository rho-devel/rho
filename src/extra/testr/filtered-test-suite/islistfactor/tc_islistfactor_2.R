expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(c(TRUE, FALSE, FALSE, FALSE, FALSE), c(TRUE, TRUE, TRUE, TRUE, NA)), FALSE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

