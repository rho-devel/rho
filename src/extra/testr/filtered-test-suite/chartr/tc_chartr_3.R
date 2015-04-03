expected <- eval(parse(text="\"MiweD FAsE 123\""));    
test(id=0, code={    
argv <- eval(parse(text="list(\"a-cX\", \"D-Fw\", \"MiXeD cAsE 123\")"));    
.Internal(chartr(argv[[1]], argv[[2]], argv[[3]]));    
}, o=expected);    

