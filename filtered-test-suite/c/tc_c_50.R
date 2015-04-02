expected <- eval(parse(text="list(1, 1, 1)"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(1, 1, 1, list())"));                  
do.call(`c`, argv);                  
}, o=expected);                  

