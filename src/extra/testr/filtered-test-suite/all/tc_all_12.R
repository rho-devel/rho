expected <- eval(parse(text="NA"));            
test(id=0, code={            
argv <- eval(parse(text="list(NA)"));            
do.call(`all`, argv);            
}, o=expected);            

