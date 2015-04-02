expected <- eval(parse(text="structure(c(1.93536640873922, 0.986800182066523), .Dim = 2L, .Dimnames = list(c(\"1\", \"2\")))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(25.1597633136098, 12.8284023668648), .Dim = 2L, .Dimnames = list(c(\"1\", \"2\"))), c(13L, 13L))"));               
do.call(`/`, argv);               
}, o=expected);               

