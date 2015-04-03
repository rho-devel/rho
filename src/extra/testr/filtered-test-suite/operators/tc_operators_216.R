expected <- eval(parse(text="0+6.28318530717959i"));     
test(id=0, code={     
argv <- eval(parse(text="list(0+2i, 3.14159265358979)"));     
do.call(`*`, argv);     
}, o=expected);     

