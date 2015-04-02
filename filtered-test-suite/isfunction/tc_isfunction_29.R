expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(2L, 1L, 3L), .Label = c(\"1\", \"2\", NA), class = c(\"ordered\", \"factor\")))"));      
do.call(`is.function`, argv);      
}, o=expected);      

