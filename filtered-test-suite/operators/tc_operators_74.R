expected <- eval(parse(text="c(0.5, -0.5)"));             
test(id=0, code={             
argv <- eval(parse(text="list(0.5, -0.5)"));             
do.call(`:`, argv);             
}, o=expected);             

