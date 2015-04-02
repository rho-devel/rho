expected <- eval(parse(text="structure(c(0, 3, 3, 0), .Dim = c(2L, 2L), counts = structure(c(0L, 0L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 2L, 2L, 0L), .Dim = c(2L, 2L, 3L), .Dimnames = list(NULL, NULL, c(\"ins\", \"del\", \"sub\"))), trafos = structure(c(\"MMMMMM\", \"SMMMSMD\", \"SMMMSMI\", \"MMMMMMM\"), .Dim = c(2L, 2L)))"));  
test(id=0, code={  
argv <- eval(parse(text="list(list(c(107L, 105L, 116L, 116L, 101L, 110L), c(115L, 105L, 116L, 116L, 105L, 110L, 103L)), list(c(107L, 105L, 116L, 116L, 101L, 110L), c(115L, 105L, 116L, 116L, 105L, 110L, 103L)), structure(c(1, 1, 1), .Names = c(\"insertions\", \"deletions\", \"substitutions\")), TRUE, TRUE, FALSE, FALSE, FALSE)"));  
.Internal(adist(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));  
}, o=expected);  

