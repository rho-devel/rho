expected <- eval(parse(text="c(1.2e+100, 1.3e+100)"));            
test(id=0, code={            
argv <- eval(parse(text="list(1.2e+100, 1.3e+100, length.out = 2)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

