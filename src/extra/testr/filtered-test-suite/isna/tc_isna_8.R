expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(2L, 5L), .Dimnames = list(NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\")))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(23L, 24L, 47L, 48L, 71L, 72L, 95L, 96L, 119L, 120L), .Dim = c(2L, 5L), .Dimnames = list(NULL, c(\"V5\", \"V6\", \"V7\", \"V8\", \"V9\"))))"));        
do.call(`is.na`, argv);        
}, o=expected);        

