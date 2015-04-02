expected <- eval(parse(text="c(NA_integer_, NA_integer_)"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(c(0, 1), .Names = c(\"Domestic\", \"Foreign\")), NA_integer_, NA_integer_, NULL)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

