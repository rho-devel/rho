expected <- eval(parse(text="c(-1, -0.5, 0, 0.5, 1)"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(-1, -0.5, 0, 0.5, 1))"));             
do.call(`invisible`, argv);             
}, o=expected);             

