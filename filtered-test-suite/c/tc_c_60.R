expected <- eval(parse(text="c(\"ArgMethod\", \"1.10714871779409\")"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(\"ArgMethod\", 1.10714871779409)"));                  
do.call(`c`, argv);                  
}, o=expected);                  

