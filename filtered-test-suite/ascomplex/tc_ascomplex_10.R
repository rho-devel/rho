expected <- eval(parse(text="1+0i"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(a = 1), .Names = \"a\"))"));   
do.call(`as.complex`, argv);   
}, o=expected);   

