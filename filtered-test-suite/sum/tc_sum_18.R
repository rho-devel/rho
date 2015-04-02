expected <- eval(parse(text="NA_integer_"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(1073741824L, 1073741824L))"));       
do.call(`sum`, argv);       
}, o=expected);       

