expected <- eval(parse(text="39L"));              
test(id=0, code={              
argv <- eval(parse(text="list(39)"));              
do.call(`as.integer`, argv);              
}, o=expected);              

