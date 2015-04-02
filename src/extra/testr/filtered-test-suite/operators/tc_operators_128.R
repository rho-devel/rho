expected <- eval(parse(text="TRUE"));            
test(id=0, code={            
argv <- eval(parse(text="list(2L, e2 = 2)"));            
do.call(`==`, argv);            
}, o=expected);            

