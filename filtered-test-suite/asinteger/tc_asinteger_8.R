expected <- eval(parse(text="-1L"));              
test(id=0, code={              
argv <- eval(parse(text="list(\"-1\")"));              
do.call(`as.integer`, argv);              
}, o=expected);              

