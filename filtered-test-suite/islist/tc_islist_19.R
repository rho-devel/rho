expected <- eval(parse(text="FALSE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(3.14159265358979e+20)"));                  
do.call(`is.list`, argv);                  
}, o=expected);                  

