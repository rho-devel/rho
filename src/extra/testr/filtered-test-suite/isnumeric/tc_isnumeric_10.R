expected <- eval(parse(text="TRUE"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1L, 2L, NA, 3L), .Label = c(\"aa\", \"bb\", \"dd\")))"));               
do.call(`is.numeric`, argv);               
}, o=expected);               

