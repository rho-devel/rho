expected <- eval(parse(text="integer(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(integer(0), class = \"hexmode\"), structure(integer(0), class = \"hexmode\"))"));   
.Internal(bitwiseAnd(argv[[1]], argv[[2]]));   
}, o=expected);   

