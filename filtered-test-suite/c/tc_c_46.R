expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(TRUE, NULL)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

