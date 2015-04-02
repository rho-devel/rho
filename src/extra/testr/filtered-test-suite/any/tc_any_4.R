expected <- eval(parse(text="TRUE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE, TRUE), .Names = c(\"1\", \"2\", \"3\", \"4\", \"5\"), .Dim = 5L, .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\"))))"));             
do.call(`any`, argv);             
}, o=expected);             

