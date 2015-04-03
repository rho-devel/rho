expected <- eval(parse(text="c(\"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"1\", \"2\", \"2\", \"2\", \"2\", \"2\", \"2\", \"2\", \"2\", \"2\", \"2\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Dim = c(10L, 2L)))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

