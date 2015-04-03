expected <- eval(parse(text="c(\".Call\", \".Call.numParameters\", \".Fortran\", \".Fortran.numParameters\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\".Call\", \".Call numParameters\", \".Fortran\", \".Fortran numParameters\"), TRUE)"));        
.Internal(make.names(argv[[1]], argv[[2]]));        
}, o=expected);        

