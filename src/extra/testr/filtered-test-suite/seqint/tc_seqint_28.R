expected <- eval(parse(text="1"));            
test(id=0, code={            
argv <- eval(parse(text="list(1, 1, by = 1)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

