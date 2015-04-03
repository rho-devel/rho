expected <- eval(parse(text="NULL"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(recursive = TRUE)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

