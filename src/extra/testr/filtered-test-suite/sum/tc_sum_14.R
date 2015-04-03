expected <- eval(parse(text="NA_real_"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(c(49, 61, NA, NA))"));                 
do.call(`sum`, argv);                 
}, o=expected);                 

