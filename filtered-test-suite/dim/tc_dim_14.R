expected <- eval(parse(text="NULL"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(c(99, 1, 2, -3, 4, 3, NA))"));                  
do.call(`dim`, argv);                  
}, o=expected);                  

