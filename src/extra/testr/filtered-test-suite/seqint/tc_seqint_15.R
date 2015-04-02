expected <- eval(parse(text="integer(0)"));            
test(id=0, code={            
argv <- eval(parse(text="list(list())"));            
do.call(`seq.int`, argv);            
}, o=expected);            

