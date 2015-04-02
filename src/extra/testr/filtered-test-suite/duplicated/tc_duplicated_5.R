expected <- eval(parse(text="logical(0)"));           
test(id=0, code={           
argv <- eval(parse(text="list(character(0), FALSE, FALSE, NA)"));           
.Internal(duplicated(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));           
}, o=expected);           

