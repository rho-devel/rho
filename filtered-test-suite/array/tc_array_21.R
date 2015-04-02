expected <- eval(parse(text="structure(c(NA, NA, NA, NA), .Dim = c(1L, 4L))"));     
test(id=0, code={     
argv <- eval(parse(text="list(NA, c(1, 4), NULL)"));     
.Internal(`array`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

