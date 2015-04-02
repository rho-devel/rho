expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(), .Names = character(0), row.names = integer(0), class = \"data.frame\"))"));          
do.call(`isS4`, argv);          
}, o=expected);          

