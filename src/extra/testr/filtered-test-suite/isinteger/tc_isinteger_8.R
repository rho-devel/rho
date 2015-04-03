expected <- eval(parse(text="TRUE"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(NA, 1L))"));          
do.call(`is.integer`, argv);          
}, o=expected);          

