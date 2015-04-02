expected <- eval(parse(text="\"myTst\""));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(\"myTst\", .Names = \"\"))"));  
.Internal(`basename`(argv[[1]]));  
}, o=expected);  

