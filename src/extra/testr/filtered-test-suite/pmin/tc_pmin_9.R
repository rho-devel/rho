expected <- eval(parse(text="19L"));           
test(id=0, code={           
argv <- eval(parse(text="list(FALSE, 48L, 19L)"));           
.Internal(pmin(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

