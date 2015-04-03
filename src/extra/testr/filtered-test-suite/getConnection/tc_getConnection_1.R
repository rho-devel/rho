expected <- eval(parse(text="structure(0L, class = c(\"terminal\", \"connection\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE)"));  
.Internal(getConnection(argv[[1]]));  
}, o=expected);  

