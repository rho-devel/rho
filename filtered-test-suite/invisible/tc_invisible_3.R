expected <- eval(parse(text="quote(Y ~ X)"));      
test(id=0, code={      
argv <- eval(parse(text="list(quote(Y ~ X))"));      
do.call(`invisible`, argv);      
}, o=expected);      

