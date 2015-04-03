expected <- eval(parse(text="logical(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(NULL)"));  
do.call(`as.logical`, argv);  
}, o=expected);  

