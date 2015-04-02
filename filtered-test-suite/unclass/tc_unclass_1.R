expected <- eval(parse(text="c(-1, -1)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(-1, -1))"));     
do.call(`unclass`, argv);     
}, o=expected);     

