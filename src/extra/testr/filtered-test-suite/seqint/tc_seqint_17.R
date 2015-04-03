expected <- eval(parse(text="0.070740277703696"));            
test(id=0, code={            
argv <- eval(parse(text="list(from = 0.070740277703696, to = 0.793110173512391, length.out = NULL)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

