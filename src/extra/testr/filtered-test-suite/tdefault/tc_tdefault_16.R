expected <- eval(parse(text="structure(1.28578345790245, .Dim = c(1L, 1L))"));             
test(id=0, code={             
argv <- eval(parse(text="list(1.28578345790245)"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

