expected <- eval(parse(text="c(NA, 1+2i)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(NA, 1+2i)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

