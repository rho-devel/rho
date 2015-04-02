expected <- eval(parse(text="structure(c(5.11111111111111, 4.11111111111111, 4.11111111111111, 3.11111111111111, 3.11111111111111, 4.11111111111111), .Dim = 6L, .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\")))"));               
test(id=0, code={               
argv <- eval(parse(text="list(2.11111111111111, structure(c(3, 2, 2, 1, 1, 2), .Dim = 6L, .Dimnames = list(c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\"))))"));               
do.call(`+`, argv);               
}, o=expected);               

