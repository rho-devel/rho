expected <- eval(parse(text="structure(list(), .Names = character(0), row.names = integer(0), class = \"data.frame\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(list(), .Names = character(0), row.names = integer(0), class = \"data.frame\"))"));     
do.call(`(`, argv);     
}, o=expected);     

