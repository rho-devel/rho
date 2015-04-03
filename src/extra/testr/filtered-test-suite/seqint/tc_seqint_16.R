expected <- eval(parse(text="c(-0.2, -2.77555756156289e-17, 0.2, 0.4, 0.6, 0.8, 1)"));            
test(id=0, code={            
argv <- eval(parse(text="list(-0.2, 1, length.out = 7)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

