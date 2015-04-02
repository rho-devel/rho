expected <- eval(parse(text="structure(c(FALSE, FALSE, TRUE, FALSE), .Names = c(\"(Intercept)\", \"x1\", \"x2\", \"x3\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(1.47191076131574, 0.586694550701453, NA, 0.258706725324317), .Names = c(\"(Intercept)\", \"x1\", \"x2\", \"x3\")))"));        
do.call(`is.na`, argv);        
}, o=expected);        

