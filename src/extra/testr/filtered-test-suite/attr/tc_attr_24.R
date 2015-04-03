expected <- eval(parse(text="structure(c(\"(Intr)\", \"-0.555\"), .Dim = c(2L, 1L), .Dimnames = list(c(\"(Intercept)\", \"day\"), \"Corr\"))"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(c(804.851443135267, 3.3994157758076, 28.3699038266834, 1.84375046462573), .Dim = c(2L, 2L), .Dimnames = list(c(\"(Intercept)\", \"day\"), c(\"Variance\", \"StdDev\")), formStr = \"pdLogChol(day)\", corr = structure(c(\"(Intr)\", \"-0.555\"), .Dim = c(2L, 1L), .Dimnames = list(c(\"(Intercept)\", \"day\"), \"Corr\"))), which = \"corr\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

