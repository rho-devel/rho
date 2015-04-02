expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(-0.6, -0.6, -0.6, -0.6, -0.6, -0.6, -0.6, -0.6, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, -0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.3, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6, 0.6))"));          
do.call(`is.integer`, argv);          
}, o=expected);          

