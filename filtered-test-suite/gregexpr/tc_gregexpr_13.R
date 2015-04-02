expected <- eval(parse(text="list(structure(c(3L, 20L), match.length = c(12L, 15L), useBytes = TRUE, capture.start = structure(c(3L, 20L, 7L, 30L), .Dim = c(2L, 2L), .Dimnames = list(NULL, c(\"first\", \"last\"))), capture.length = structure(c(3L, 9L, 8L, 5L), .Dim = c(2L, 2L), .Dimnames = list(NULL, c(\"first\", \"last\"))), capture.names = c(\"first\", \"last\")), structure(2L, match.length = 16L, useBytes = TRUE, capture.start = structure(c(2L, 10L), .Dim = 1:2, .Dimnames = list(NULL, c(\"first\", \"last\"))), capture.length = structure(7:8, .Dim = 1:2, .Dimnames = list(    NULL, c(\"first\", \"last\"))), capture.names = c(\"first\", \"last\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)\", c(\"  Ben Franklin and Jefferson Davis\", \"\\tMillard Fillmore\"), FALSE, TRUE, FALSE, FALSE)"));      
.Internal(gregexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));      
}, o=expected);      

