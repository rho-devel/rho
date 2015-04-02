expected <- eval(parse(text="structure(c(TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE), .Dim = 3:4)"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), .Dim = 3:4), structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L), .Dim = 3:4))"));            
do.call(`==`, argv);            
}, o=expected);            

