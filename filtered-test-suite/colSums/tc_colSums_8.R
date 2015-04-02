expected <- eval(parse(text="c(0, 1)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(0:1, .Dim = 1:2, .Dimnames = list(\"strata(grp)\", c(\"x\", \"strata(grp)\"))), 1, 2, FALSE)"));        
.Internal(colSums(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

