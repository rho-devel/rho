expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(1, 1, 3, 1, 1, 3, 3, 3, 3), FALSE, NULL)"));            
do.call(`all`, argv);            
}, o=expected);            

