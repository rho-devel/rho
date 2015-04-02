expected <- eval(parse(text="structure(c(0L, 0L, 1L, 1L), .Names = c(\"y\", \"x\", \"Ta\", \"Tb\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(52L, 52L, 1L, 1L), .Names = c(\"y\", \"x\", \"Ta\", \"Tb\")), 52L)"));              
do.call(`%%`, argv);              
}, o=expected);              

