expected <- eval(parse(text="structure(c(0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L), .Dim = c(1L, 8L), .Dimnames = list(\"strata(enum)\", c(\"rx\", \"size\", \"number\", \"strata(enum)\", \"cluster(id)\", \"rx:strata(enum)\", \"size:strata(enum)\", \"number:strata(enum)\")))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(0L, 0L, 0L, 1L, 0L, 1L, 1L, 1L), .Dim = c(1L, 8L), .Dimnames = list(\"strata(enum)\", c(\"rx\", \"size\", \"number\", \"strata(enum)\", \"cluster(id)\", \"rx:strata(enum)\", \"size:strata(enum)\", \"number:strata(enum)\"))), 1:2, TRUE)"));           
.Internal(aperm(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

