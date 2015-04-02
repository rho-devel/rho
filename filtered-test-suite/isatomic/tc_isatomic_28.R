expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(a1 = 1:3, a2 = 4:6, a3 = 3.14159265358979, a4 = c(\"a\", \"b\", \"c\")), .Names = c(\"a1\", \"a2\", \"a3\", \"a4\")))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

