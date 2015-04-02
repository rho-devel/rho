expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(class = c(\"ordered\", \"factor\"), levels = character(0)), .Names = c(\"class\", \"levels\")))"));              
do.call(`is.expression`, argv);              
}, o=expected);              

