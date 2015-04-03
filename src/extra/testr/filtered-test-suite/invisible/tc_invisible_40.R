expected <- eval(parse(text="c(1e-10, 1e+49, 1e+108, 1e+167, 1e+226)"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(1e-10, 1e+49, 1e+108, 1e+167, 1e+226))"));             
do.call(`invisible`, argv);             
}, o=expected);             

