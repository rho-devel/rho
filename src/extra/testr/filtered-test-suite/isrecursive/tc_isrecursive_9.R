expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(1e+09)"));            
do.call(`is.recursive`, argv);            
}, o=expected);            

