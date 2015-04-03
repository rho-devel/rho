expected <- eval(parse(text="integer(0)"));              
test(id=0, code={              
argv <- eval(parse(text="list(NULL)"));              
do.call(`as.integer`, argv);              
}, o=expected);              

