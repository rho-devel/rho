expected <- eval(parse(text="NaN"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE)"));  
do.call(`acosh`, argv);  
}, o=expected);  

