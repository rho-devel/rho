expected <- eval(parse(text="2L"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE)"));  
.Internal(sink.number(argv[[1]]));  
}, o=expected);  

