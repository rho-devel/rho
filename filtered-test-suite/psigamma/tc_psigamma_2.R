expected <- eval(parse(text="c(-1e-60, -1.00000000000001e-90, -1.00000000000001e-120, -1.00000000000001e-150, -1.00000000000001e-180)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1e+30, 1e+45, 1e+60, 1e+75, 1e+90), 2)"));  
.Internal(psigamma(argv[[1]], argv[[2]]));  
}, o=expected);  

