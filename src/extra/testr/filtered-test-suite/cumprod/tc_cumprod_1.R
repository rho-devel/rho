expected <- eval(parse(text="structure(c(1, 60, 3600, 86400, 604800), .Names = c(\"secs\", \"mins\", \"hours\", \"days\", \"weeks\"))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(1, 60, 60, 24, 7), .Names = c(\"secs\", \"mins\", \"hours\", \"days\", \"weeks\")))"));  
do.call(`cumprod`, argv);  
}, o=expected);  

