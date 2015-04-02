expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(NA, 6346.2), .Names = c(\"1\", \"2\")))"));              
do.call(`is.expression`, argv);              
}, o=expected);              

