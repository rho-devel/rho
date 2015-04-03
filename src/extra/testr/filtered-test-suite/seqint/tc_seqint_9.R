expected <- eval(parse(text="integer(0)"));            
test(id=0, code={            
argv <- eval(parse(text="list(from = 0, to = 0.793110173512391, length.out = FALSE)"));            
do.call(`seq.int`, argv);            
}, o=expected);            

