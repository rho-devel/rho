expected <- eval(parse(text="c(1L, 0L, 0L)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(2, 0, 0, 0, 0, 1, 0, 0, 0, 0, 3, 0, 6, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Names = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\", \"k\", \"l\", \"m\", \"n\", \"o\", \"p\", \"q\", \"r\", \"s\", \"t\", \"u\", \"v\", \"w\", \"x\", \"y\")), NULL, 0L)"));         
.Internal(format.info(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

