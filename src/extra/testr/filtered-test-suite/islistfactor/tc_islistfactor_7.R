expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(c(\"     The binary arithmetic operators are generic functions: methods can\", \"     be written for them individually or via the ‘Ops’ group generic\", \"     function.  (See ‘Ops’ for how dispatch is computed.)\")), TRUE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

