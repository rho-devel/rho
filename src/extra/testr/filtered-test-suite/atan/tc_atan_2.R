expected <- eval(parse(text="c(-1.5707963267949, 1.5707963267949)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(-Inf, Inf))"));     
do.call(`atan`, argv);     
}, o=expected);     

