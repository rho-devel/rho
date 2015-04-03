expected <- eval(parse(text="8.79570414530351e-05"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(0.138260298853371, 0.000636169906925458))"));          
do.call(`prod`, argv);          
}, o=expected);          

