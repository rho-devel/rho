expected <- eval(parse(text="FALSE"));           
test(id=0, code={           
argv <- eval(parse(text="list(1L, FALSE, TRUE, NA)"));           
.Internal(duplicated(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));           
}, o=expected);           

