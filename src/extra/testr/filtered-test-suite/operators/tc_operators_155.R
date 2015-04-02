expected <- eval(parse(text="structure(c(TRUE, TRUE, FALSE, TRUE), .Names = c(\"1\", \"2\", \"3\", \"4\"))"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(0L, 1L, 1L, 3L), .Names = c(\"1\", \"2\", \"3\", \"4\")), 0:3)"));          
do.call(`>=`, argv);          
}, o=expected);          

