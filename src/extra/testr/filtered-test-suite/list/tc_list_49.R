expected <- eval(parse(text="list(\"‘\", \"Matrix\", \"’\")"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(\"‘\", \"Matrix\", \"’\")"));                  
do.call(`list`, argv);                  
}, o=expected);                  

