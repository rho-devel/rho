expected <- eval(parse(text="c(1, 0, 0, 0)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(1, FALSE, c(0, 0))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

