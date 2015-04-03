expected <- eval(parse(text="structure(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), .Names = c(\"1\", \"3\", \"5\", \"7\", \"9\", \"11\", \"13\", \"15\", \"17\", \"19\", \"21\", \"23\"))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 3.5527136788005e-15, 0, 0), .Names = c(\"1\", \"3\", \"5\", \"7\", \"9\", \"11\", \"13\", \"15\", \"17\", \"19\", \"21\", \"23\")), 0.001)"));   
do.call(`>`, argv);   
}, o=expected);   

