expected <- eval(parse(text="0L"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(-1.001, -1, -0.999), FALSE, FALSE)"));     
.Internal(`anyDuplicated`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

