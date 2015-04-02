expected <- eval(parse(text="568.628270299753"));                
test(id=0, code={                
argv <- eval(parse(text="list(181L, 3.14159265358979)"));                
do.call(`*`, argv);                
}, o=expected);                

