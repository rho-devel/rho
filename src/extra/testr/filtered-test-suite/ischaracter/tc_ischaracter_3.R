expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(usr = c(-0.04, 1.04, -0.04, 1.04), mgp = c(3, 1, 0)), .Names = c(\"usr\", \"mgp\")))"));   
do.call(`is.character`, argv);   
}, o=expected);   

