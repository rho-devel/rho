expected <- eval(parse(text="\"3.14159E-06\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"%G\", 3.14159265358979e-06)"));           
.Internal(sprintf(argv[[1]], argv[[2]]));           
}, o=expected);           

