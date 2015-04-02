expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(2L)"));  
.Internal(.getRestart(argv[[1]]));  
}, o=expected);  

