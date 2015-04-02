expected <- eval(parse(text="NULL"));  
test(id=0, code={  
argv <- eval(parse(text="list(FALSE, FALSE)"));  
do.call(`retracemem`, argv);  
}, o=expected);  

