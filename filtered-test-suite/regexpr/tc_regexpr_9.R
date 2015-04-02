expected <- eval(parse(text="structure(c(3L, 2L), match.length = c(12L, 16L), useBytes = TRUE, capture.start = structure(c(3L, 2L, 7L, 10L), .Dim = c(2L, 2L), .Dimnames = list(NULL, c(\"first\", \"last\"))), capture.length = structure(c(3L, 7L, 8L, 8L), .Dim = c(2L, 2L), .Dimnames = list(NULL, c(\"first\", \"last\"))), capture.names = c(\"first\", \"last\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"(?<first>[[:upper:]][[:lower:]]+) (?<last>[[:upper:]][[:lower:]]+)\", c(\"  Ben Franklin and Jefferson Davis\", \"\\tMillard Fillmore\"), FALSE, TRUE, FALSE, FALSE)"));           
.Internal(regexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
}, o=expected);           

