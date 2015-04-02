expected <- eval(parse(text="structure(0.0812163064507375, .Names = \"Var2\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(0.142857142857143, .Names = \"Var2\"), structure(1.75510204081633, .Names = \"Var1\"))"));          
.Internal(atan2(argv[[1]], argv[[2]]));          
}, o=expected);          

