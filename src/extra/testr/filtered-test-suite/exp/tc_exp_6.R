expected <- eval(parse(text="c(0.90483741803596, 0.042329219623205, 3.72007597602068e-44)"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(-0.1, -3.16227766016838, -100))"));   
do.call(`exp`, argv);   
}, o=expected);   

