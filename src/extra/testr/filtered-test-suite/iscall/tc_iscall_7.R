expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(2.828, -1.04, -2.738, 3.084), .Names = c(\"(Intercept)\", \"age\", \"wgt\", \"prot\")))"));              
do.call(`is.call`, argv);              
}, o=expected);              

