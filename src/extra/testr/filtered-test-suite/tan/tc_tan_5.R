expected <- eval(parse(text="0+1i"));  
test(id=0, code={  
argv <- eval(parse(text="list(1+1000i)"));  
do.call(`tan`, argv);  
}, o=expected);  

