expected <- eval(parse(text="c(0.258706725324317, 1.47191076131574)"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(1.47191076131574, 0.586694550701453, 0.258706725324317, 0.948371836939988, 0.396080061109718, 0.350912037541581), finite = TRUE, na.rm = FALSE)"));    
do.call(`range`, argv);    
}, o=expected);    

