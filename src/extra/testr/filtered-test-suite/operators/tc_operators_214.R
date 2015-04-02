expected <- eval(parse(text="0+2i"));     
test(id=0, code={     
argv <- eval(parse(text="list(0+1i, 2)"));     
do.call(`*`, argv);     
}, o=expected);     

