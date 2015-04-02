expected <- eval(parse(text="FALSE"));           
test(id=0, code={           
argv <- eval(parse(text="list(FALSE, FALSE)"));           
.Internal(pmin(argv[[1]], argv[[2]]));           
}, o=expected);           

