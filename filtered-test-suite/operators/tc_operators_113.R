expected <- eval(parse(text="structure(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE), .Dim = 12L)"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2, 0), .Dim = 12L), 0)"));            
do.call(`==`, argv);            
}, o=expected);            

