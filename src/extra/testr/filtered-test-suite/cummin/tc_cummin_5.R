expected <- eval(parse(text="integer(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(logical(0))"));  
do.call(`cummin`, argv);  
}, o=expected);  

