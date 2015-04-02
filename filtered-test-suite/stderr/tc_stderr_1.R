expected <- eval(parse(text="structure(2L, class = c(\"terminal\", \"connection\"))"));  
test(id=0, code={  
.Internal(`stderr`());  
}, o=expected);  

