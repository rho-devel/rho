expected <- eval(parse(text="1:4"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"y\", \"A\", \"U\", \"V\"))"));        
do.call(`seq_along`, argv);        
}, o=expected);        

