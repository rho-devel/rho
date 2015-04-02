expected <- eval(parse(text="10L"));     
test(id=0, code={     
argv <- eval(parse(text="list(2L, 5L)"));     
do.call(`*`, argv);     
}, o=expected);     

