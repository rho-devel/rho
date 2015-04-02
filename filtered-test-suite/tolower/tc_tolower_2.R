expected <- eval(parse(text="\"true\""));       
test(id=0, code={       
argv <- eval(parse(text="list(\"TRUE\")"));       
.Internal(tolower(argv[[1]]));       
}, o=expected);       

