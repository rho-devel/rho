expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE), .Dim = c(5L, 2L), .Dimnames = list(NULL, c(\"VAR1\", \"VAR3\")))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(VAR1 = c(1, 2, 3, 4, 5), VAR3 = c(1, 1, 1, 1, NA)), .Names = c(\"VAR1\", \"VAR3\"), class = \"data.frame\", row.names = c(NA, -5L)))"));        
do.call(`is.na`, argv);        
}, o=expected);        

