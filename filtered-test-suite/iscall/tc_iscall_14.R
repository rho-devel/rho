expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(10L, 10L, 10L, 10L, 10L), .Dim = 5L, .Dimnames = structure(list(a = c(\"0.333333333333333\", \"0.5\", \"1\", \"Inf\", NA)), .Names = \"a\")))"));              
do.call(`is.call`, argv);              
}, o=expected);              

