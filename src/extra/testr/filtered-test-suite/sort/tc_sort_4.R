expected <- eval(parse(text="c(\"graphics\", \"lattice\", \"stats\")"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(\"graphics\", \"lattice\", \"stats\"), FALSE)"));            
.Internal(sort(argv[[1]], argv[[2]]));            
}, o=expected);            

