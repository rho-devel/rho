expected <- eval(parse(text="c(0-3i, 0-2i, 0-1i, 0+0i, 0+1i, 0+2i, 0+3i)"));                
test(id=0, code={                
argv <- eval(parse(text="list(-3:3, 0+1i)"));                
do.call(`*`, argv);                
}, o=expected);                

