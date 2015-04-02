expected <- eval(parse(text="character(0)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(), .Names = character(0), row.names = integer(0), class = \"data.frame\"))"));         
do.call(`names`, argv);         
}, o=expected);         

