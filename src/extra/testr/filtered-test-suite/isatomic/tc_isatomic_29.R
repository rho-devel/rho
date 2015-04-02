expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(2L, 1L, 3L), .Label = c(\"1\", \"2\", NA), class = \"factor\"))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

