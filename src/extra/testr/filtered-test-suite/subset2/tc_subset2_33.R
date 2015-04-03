expected <- eval(parse(text="structure(c(16L, 16L, 144L, 16L, 16L, 128L, 16L, 16L, 112L, 16L), .Dim = 10L, .Dimnames = structure(list(c(\"1\", \"6\", \"7\", \"8\", \"13\", \"14\", \"15\", \"20\", \"21\", \"22\")), .Names = \"\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(variog = c(0.00723952158228125, 0.014584633605134, 0.0142079356273193, 0.0184422668389517, 0.0111285046171491, 0.0199100817701382, 0.0270723108677323, 0.0341403794476899, 0.0283206569034573, 0.03752550654923), dist = c(1, 6, 7, 8, 13, 14, 15, 20, 21, 22), n.pairs = structure(c(16L, 16L, 144L, 16L, 16L, 128L, 16L, 16L, 112L, 16L), .Dim = 10L, .Dimnames = structure(list(c(\"1\", \"6\", \"7\", \"8\", \"13\", \"14\", \"15\", \"20\", \"21\", \"22\")), .Names = \"\"))), .Names = c(\"variog\", \"dist\", \"n.pairs\"), collapse = TRUE, row.names = c(NA, 10L), class = c(\"Variogram\", \"data.frame\")), 3L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

