expected <- eval(parse(text="61"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(raw(0), 61)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

