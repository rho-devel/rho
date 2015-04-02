expected <- eval(parse(text="quote(breaks ~ (wool + tension)^2)"));      
test(id=0, code={      
argv <- eval(parse(text="list(quote(breaks ~ (wool + tension)^2))"));      
do.call(`invisible`, argv);      
}, o=expected);      

