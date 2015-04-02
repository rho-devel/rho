expected <- eval(parse(text="quote(~a + b:c + d + e + e:d)"));      
test(id=0, code={      
argv <- eval(parse(text="list(quote(~a + b:c + d + e + e:d))"));      
do.call(`invisible`, argv);      
}, o=expected);      

