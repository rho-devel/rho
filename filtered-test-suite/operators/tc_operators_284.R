expected <- eval(parse(text="structure(c(0L, 0L), .Names = c(\"y\", \"x\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(8L, 8L), .Names = c(\"y\", \"x\")), 8L)"));           
do.call(`%%`, argv);           
}, o=expected);           

