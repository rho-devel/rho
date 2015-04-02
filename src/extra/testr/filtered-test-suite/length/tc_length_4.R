expected <- eval(parse(text="1L"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(a = 6:10), .Names = \"a\", row.names = 6:10, class = \"data.frame\"))"));          
do.call(`length`, argv);          
}, o=expected);          

