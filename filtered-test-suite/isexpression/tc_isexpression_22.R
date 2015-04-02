expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(dim = 1L, dimnames = list(\"a\")), .Names = c(\"dim\", \"dimnames\")))"));              
do.call(`is.expression`, argv);              
}, o=expected);              

