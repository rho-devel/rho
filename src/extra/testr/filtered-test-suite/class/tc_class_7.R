expected <- eval(parse(text="\"character\""));              
test(id=0, code={              
argv <- eval(parse(text="list(c(NA, \"2\", \"3\"))"));              
do.call(`class`, argv);              
}, o=expected);              

