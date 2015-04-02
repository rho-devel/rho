expected <- eval(parse(text="c(1+8i, 2+9i)"));               
test(id=0, code={               
argv <- eval(parse(text="list(1:2, c(0+8i, 0+9i))"));               
do.call(`+`, argv);               
}, o=expected);               

