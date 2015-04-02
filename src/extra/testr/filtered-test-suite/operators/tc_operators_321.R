expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE), .Names = c(\"8\", \"10\", \"12\", \"14\"), .Dim = 4L, .Dimnames = list(c(\"8\", \"10\", \"12\", \"14\")))"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(FALSE, FALSE, FALSE, FALSE), .Dim = 4L, .Dimnames = list(c(\"8\", \"10\", \"12\", \"14\"))))"));          
do.call(`!`, argv);          
}, o=expected);          

