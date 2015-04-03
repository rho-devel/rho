expected <- eval(parse(text="NULL"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(c(2832L, 2836L, 2836L, 2833L, 2833L))"));                  
do.call(`dim`, argv);                  
}, o=expected);                  

