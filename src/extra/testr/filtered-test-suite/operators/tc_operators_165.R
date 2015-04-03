expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE), .Dim = c(5L, 2L), .Dimnames = list(NULL, c(\"VAR1\", \"VAR3\")))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE), .Dim = c(5L, 2L), .Dimnames = list(NULL, c(\"VAR1\", \"VAR3\"))))"));  
do.call(`!`, argv);  
}, o=expected);  

