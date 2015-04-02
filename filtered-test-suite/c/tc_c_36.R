expected <- eval(parse(text="c(-1+0i, 0+1i)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(-1, 0+1i)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

