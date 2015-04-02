expected <- eval(parse(text="\"MwheD cAyE 123\""));    
test(id=0, code={    
argv <- eval(parse(text="list(\"iXs\", \"why\", \"MiXeD cAsE 123\")"));    
.Internal(chartr(argv[[1]], argv[[2]], argv[[3]]));    
}, o=expected);    

