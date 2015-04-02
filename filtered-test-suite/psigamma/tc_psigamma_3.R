expected <- eval(parse(text="c(2.39999999999997e-99, 2.40000000000003e-149, 2.39999999999995e-199, 2.4e-249, 2.40000000000006e-299)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1e+20, 1e+30, 1e+40, 1e+50, 1e+60), 5)"));  
.Internal(psigamma(argv[[1]], argv[[2]]));  
}, o=expected);  

