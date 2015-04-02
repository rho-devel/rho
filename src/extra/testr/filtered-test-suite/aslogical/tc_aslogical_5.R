expected <- eval(parse(text="NA"));         
test(id=0, code={         
argv <- eval(parse(text="list(\"\")"));         
do.call(`as.logical`, argv);         
}, o=expected);         

