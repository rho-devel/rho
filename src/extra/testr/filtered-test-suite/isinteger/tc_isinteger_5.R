expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(NA, -4.19095158576965e-09))"));          
do.call(`is.integer`, argv);          
}, o=expected);          

