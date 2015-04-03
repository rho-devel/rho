expected <- eval(parse(text="numeric(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(list())"));  
do.call(`cummax`, argv);  
}, o=expected);  

