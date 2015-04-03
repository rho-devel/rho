expected <- eval(parse(text="-0.000700245114393392"));  
test(id=0, code={  
argv <- eval(parse(text="list(-7e-04)"));  
do.call(`log1p`, argv);  
}, o=expected);  

