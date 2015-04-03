expected <- eval(parse(text="c(TRUE, TRUE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(-4, 1), .Names = c(\"\", \"\")))"));         
do.call(`as.logical`, argv);         
}, o=expected);         

