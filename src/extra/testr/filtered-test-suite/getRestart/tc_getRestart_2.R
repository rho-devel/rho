expected <- eval(parse(text="structure(list(\"abort\", NULL), class = \"restart\")"));  
test(id=0, code={  
argv <- eval(parse(text="list(1L)"));  
.Internal(.getRestart(argv[[1]]));  
}, o=expected);  

