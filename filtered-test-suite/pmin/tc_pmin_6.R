expected <- eval(parse(text="numeric(0)"));           
test(id=0, code={           
argv <- eval(parse(text="list(FALSE, 1, structure(numeric(0), .Dim = c(4L, 0L)))"));           
.Internal(pmin(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

