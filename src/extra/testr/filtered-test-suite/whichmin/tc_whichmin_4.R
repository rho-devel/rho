expected <- eval(parse(text="integer(0)"));       
test(id=0, code={       
argv <- eval(parse(text="list(list())"));       
.Internal(which.min(argv[[1]]));       
}, o=expected);       

