expected <- eval(parse(text="32"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(4L, 4L, 2L), .Names = c(\"Hair\", \"Eye\", \"Sex\")))"));   
do.call(`prod`, argv);   
}, o=expected);   

