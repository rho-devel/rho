expected <- eval(parse(text="c(1944, 1944.75, 4)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(1944, 1944.75, 4)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

