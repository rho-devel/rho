expected <- eval(parse(text="1e+05"));         
test(id=0, code={         
argv <- eval(parse(text="list(1e+05)"));         
do.call(`ceiling`, argv);         
}, o=expected);         

