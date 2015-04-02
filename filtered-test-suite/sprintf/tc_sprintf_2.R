expected <- eval(parse(text="\"3\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"%1.0f\", 3.14159265358979)"));           
.Internal(sprintf(argv[[1]], argv[[2]]));           
}, o=expected);           

