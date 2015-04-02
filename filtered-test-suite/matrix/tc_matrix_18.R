expected <- eval(parse(text="structure(c(1448665, 18316380, 77.4, 139.8, 2251281, 26424236, 120.3, 201.7, NA, NA, 1835812, 22608335, 98.1, 172.5), .Dim = c(2L, 7L), .Dimnames = list(c(\"Ncells\", \"Vcells\"), c(\"used\", \"(Mb)\", \"gc trigger\", \"(Mb)\", \"limit (Mb)\", \"max used\", \"(Mb)\")))"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(1448665, 18316380, 77.4, 139.8, 2251281, 26424236, 120.3, 201.7, NA, NA, 1835812, 22608335, 98.1, 172.5), 2L, 7L, FALSE, list(c(\"Ncells\", \"Vcells\"), c(\"used\", \"(Mb)\", \"gc trigger\", \"(Mb)\", \"limit (Mb)\", \"max used\", \"(Mb)\")), FALSE, FALSE)"));             
.Internal(matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));             
}, o=expected);             

