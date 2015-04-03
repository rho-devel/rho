expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(25, 50, 100, 250, 500, 1e+05))"));             
do.call(`is.object`, argv);             
}, o=expected);             

