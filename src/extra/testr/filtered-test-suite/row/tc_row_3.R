expected <- eval(parse(text="structure(integer(0), .Dim = 0:1)"));       
test(id=0, code={       
argv <- eval(parse(text="list(0:1)"));       
.Internal(row(argv[[1]]));       
}, o=expected);       

