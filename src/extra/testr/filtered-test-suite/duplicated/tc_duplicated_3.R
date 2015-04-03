expected <- eval(parse(text="c(FALSE, FALSE, FALSE)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(0, 0.700492869640978, NA), FALSE, FALSE, NA)"));           
.Internal(duplicated(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));           
}, o=expected);           

