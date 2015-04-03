expected <- eval(parse(text="102:112"));            
test(id=0, code={            
argv <- eval(parse(text="list(102L, 112L, 1L)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

