expected <- eval(parse(text="list(quote(list), quote(ya), quote(x[rep.int(NA_integer_, nyy), nm.x, drop = FALSE]))"));       
test(id=0, code={       
argv <- eval(parse(text="list(quote(list(ya, x[rep.int(NA_integer_, nyy), nm.x, drop = FALSE])), \"list\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

