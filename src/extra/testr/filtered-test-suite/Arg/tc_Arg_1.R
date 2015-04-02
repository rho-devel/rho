expected <- eval(parse(text="1.10714871779409"));  
test(id=0, code={  
argv <- eval(parse(text="list(1+2i)"));  
do.call(`Arg`, argv);  
}, o=expected);  

