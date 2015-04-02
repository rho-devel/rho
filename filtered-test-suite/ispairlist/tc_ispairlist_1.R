expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(NULL, c(\"time\", \"status\")))"));  
do.call(`is.pairlist`, argv);  
}, o=expected);  

