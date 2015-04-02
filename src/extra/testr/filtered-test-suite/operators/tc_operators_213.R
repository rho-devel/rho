expected <- eval(parse(text="8"));     
test(id=0, code={     
argv <- eval(parse(text="list(1, 8)"));     
do.call(`*`, argv);     
}, o=expected);     

