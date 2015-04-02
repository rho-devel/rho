expected <- eval(parse(text="13991"));  
test(id=0, code={  
argv <- eval(parse(text="list(13990.84)"));  
do.call(`ceiling`, argv);  
}, o=expected);  

