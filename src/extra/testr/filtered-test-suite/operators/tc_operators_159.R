expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(4L, 4L))"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1), .Dim = c(4L, 4L)), 0)"));          
do.call(`>=`, argv);          
}, o=expected);          

