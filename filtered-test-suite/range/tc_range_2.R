expected <- eval(parse(text="c(52L, 52L)"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(52L, 52L), .Names = c(\"y\", \"x\")), na.rm = FALSE)"));    
do.call(`range`, argv);    
}, o=expected);    

