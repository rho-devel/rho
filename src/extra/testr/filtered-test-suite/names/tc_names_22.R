expected <- eval(parse(text="\"xlev\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(xlev = structure(list(), .Names = character(0))), .Names = \"xlev\"))"));         
do.call(`names`, argv);         
}, o=expected);         

