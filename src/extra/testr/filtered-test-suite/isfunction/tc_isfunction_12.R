expected <- eval(parse(text="FALSE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(9, 13, 13, 18, 23, 28, 31, 34, 45, 48, 161, 5, 5, 8, 8, 12, 16, 23, 27, 30, 33, 43, 45, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1), .Dim = c(23L, 2L), .Dimnames = list(NULL, c(\"time\", \"status\")), type = \"right\", class = \"Surv\"))"));      
do.call(`is.function`, argv);      
}, o=expected);      

