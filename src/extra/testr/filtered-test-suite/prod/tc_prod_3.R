expected <- eval(parse(text="-15.6875"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(4, 2.5, 1.3, -1.20673076923077))"));          
do.call(`prod`, argv);          
}, o=expected);          

