expected <- eval(parse(text="c(-0.07401461196203+2.04920150576944i, -0.07401461196203-2.04920150576944i)"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1, 0.035205614861993, 0.237828814667385), .Names = c(\"\", \"\", \"\")))"));      
.Internal(polyroot(argv[[1]]));      
}, o=expected);      

