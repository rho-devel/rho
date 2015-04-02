expected <- eval(parse(text="c(1.97479242156194, 1.71068206679967, 1.52241456554483)"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(1.97479242156194, 1.71068206679967, 1.52241456554483), .Names = c(\"Bens of Jura\", \"Knock Hill\", \"Lairig Ghru\")))"));     
do.call(`as.double`, argv);     
}, o=expected);     

