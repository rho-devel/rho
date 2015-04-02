expected <- eval(parse(text="character(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(character(0))"));  
.Internal(Sys.readlink(argv[[1]]));  
}, o=expected);  

