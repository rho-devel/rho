expected <- eval(parse(text="0L"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE)"));  
do.call(`cummin`, argv);  
}, o=expected);  

