expected <- eval(parse(text="structure(c(1.47191076131574, 0.586694550701453, NA, 0.258706725324317, 0.948371836939988, 0.396080061109718, NA, 0.350912037541581), .Dim = c(4L, 2L), .Dimnames = list(c(\"(Intercept)\", \"x1\", \"x2\", \"x3\"), c(\"Estimate\", \"Std. Error\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1.47191076131574, 0.586694550701453, NA, 0.258706725324317, 0.948371836939988, 0.396080061109718, NA, 0.350912037541581), .Dim = c(4L, 2L), .Dimnames = list(c(\"(Intercept)\", \"x1\", \"x2\", \"x3\"), c(\"Estimate\", \"Std. Error\"))))"));      
do.call(`abs`, argv);      
}, o=expected);      

