expected <- eval(parse(text="structure(c(\"MMMMMM\", \"SMMMSMD\", \"SMMMSMI\", \"MMMMMMM\"), .Dim = c(2L, 2L))"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(c(0, 3, 3, 0), .Dim = c(2L, 2L), counts = structure(c(0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 2L, 2L, 0L), .Dim = c(2L, 2L, 3L), .Dimnames = list(NULL, NULL, c(\"ins\", \"del\", \"sub\"))), trafos = structure(c(\"MMMMMM\", \"SMMMSMD\", \"SMMMSMI\", \"MMMMMMM\"), .Dim = c(2L, 2L))), \"trafos\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

