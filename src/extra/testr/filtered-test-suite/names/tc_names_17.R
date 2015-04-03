expected <- eval(parse(text="NULL"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(c(1281L, 1283L))"));                   
do.call(`names`, argv);                   
}, o=expected);                   

