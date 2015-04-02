expected <- eval(parse(text="\"[0,5]\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"(0,5]\", 1L, 1L, \"[\")"));  
.Internal('substr<-'(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

