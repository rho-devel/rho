expected <- eval(parse(text="c(0, 1, 1.3, 1.8, 2.4, 4.6)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(c(0, 1, 1.3, 1.8, 2.4), 4.6, NULL)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

