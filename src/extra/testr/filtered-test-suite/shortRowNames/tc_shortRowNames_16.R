expected <- eval(parse(text="10L"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(variog = structure(c(\"0.007239522\", \"0.014584634\", \"0.014207936\", \"0.018442267\", \"0.011128505\", \"0.019910082\", \"0.027072311\", \"0.034140379\", \"0.028320657\", \"0.037525507\"), class = \"AsIs\"), dist = structure(c(\" 1\", \" 6\", \" 7\", \" 8\", \"13\", \"14\", \"15\", \"20\", \"21\", \"22\"), class = \"AsIs\"), n.pairs = structure(c(\" 16\", \" 16\", \"144\", \" 16\", \" 16\", \"128\", \" 16\", \" 16\", \"112\", \" 16\"), .Dim = 10L, .Dimnames = structure(list(c(\"1\", \"6\", \"7\", \"8\", \"13\", \"14\", \"15\", \"20\", \"21\", \"22\")), .Names = \"\"))), .Names = c(\"variog\", \"dist\", \"n.pairs\"), row.names = c(\"1\", \"2\", \"3\", \"4\", \"5\", \"6\", \"7\", \"8\", \"9\", \"10\"), class = \"data.frame\"), 2L)"));                 
.Internal(shortRowNames(argv[[1]], argv[[2]]));                 
}, o=expected);                 

