expected <- eval(parse(text="structure(c(8.5965311794562, 7.5965311794562, 7.5965311794562, 6.5965311794562, 6.5965311794562, 7.5965311794562), .Dim = 6L, .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\")))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(5.5965311794562, .Names = \"thetas\"), structure(c(3, 2, 2, 1, 1, 2), .Dim = 6L, .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\"))))"));               
do.call(`+`, argv);               
}, o=expected);               

