expected <- eval(parse(text="2.25"));                
test(id=0, code={                
argv <- eval(parse(text="list(1.5, 1.5)"));                
do.call(`*`, argv);                
}, o=expected);                

