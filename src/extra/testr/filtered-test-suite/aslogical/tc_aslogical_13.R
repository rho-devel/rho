expected <- eval(parse(text="TRUE"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(a = 1), .Names = \"a\"))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

