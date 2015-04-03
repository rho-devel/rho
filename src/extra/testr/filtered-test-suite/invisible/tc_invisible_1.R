expected <- eval(parse(text="c(3.14159265358977, 3.14159265358981)"));      
test(id=0, code={      
argv <- eval(parse(text="list(c(3.14159265358977, 3.14159265358981))"));      
do.call(`invisible`, argv);      
}, o=expected);      

