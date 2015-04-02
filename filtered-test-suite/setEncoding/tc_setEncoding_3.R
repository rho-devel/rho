expected <- eval(parse(text="\"3.0.1\""));       
test(id=0, code={       
argv <- eval(parse(text="list(\"3.0.1\", \"unknown\")"));       
.Internal(setEncoding(argv[[1]], argv[[2]]));       
}, o=expected);       

