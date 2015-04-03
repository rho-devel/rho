expected <- eval(parse(text="1e+06"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(1000L, 1000L))"));          
do.call(`prod`, argv);          
}, o=expected);          

