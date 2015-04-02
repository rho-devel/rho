expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(0.5)"));  
.Internal(Sys.sleep(argv[[1]]));  
}, o=expected);  

