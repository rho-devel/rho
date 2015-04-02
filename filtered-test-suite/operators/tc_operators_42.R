expected <- eval(parse(text="structure(c(2.40236686390472, 12.0828402366869), .Dim = 2L, .Dimnames = list(c(\"1\", \"2\")))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(6316.53846153846, 6350.69230769231), .Dim = 2L, .Dimnames = list(c(\"1\", \"2\"))), c(6314.13609467456, 6338.60946745562))"));               
do.call(`-`, argv);               
}, o=expected);               

