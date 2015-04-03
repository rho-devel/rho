expected <- eval(parse(text="1:25"));            
test(id=0, code={            
argv <- eval(parse(text="list(25L)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

