expected <- eval(parse(text="0L"));              
test(id=0, code={              
argv <- eval(parse(text="list(429204532L, 2L)"));              
do.call(`%%`, argv);              
}, o=expected);              

