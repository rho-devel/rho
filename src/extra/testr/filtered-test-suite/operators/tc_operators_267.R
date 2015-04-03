expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Dim = c(12L, 2L), .Dimnames = list(c(\"1\", \"3\", \"5\", \"7\", \"9\", \"11\", \"13\", \"15\", \"17\", \"19\", \"21\", \"23\"), c(\"X\", \"M\")))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), .Dim = c(12L, 2L), .Dimnames = list(c(\"1\", \"3\", \"5\", \"7\", \"9\", \"11\", \"13\", \"15\", \"17\", \"19\", \"21\", \"23\"), c(\"X\", \"M\"))), 0.001)"));   
do.call(`>`, argv);   
}, o=expected);   

