expected <- eval(parse(text="FALSE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(Plant = structure(c(5L, 5L, 5L, 5L, 5L, 5L, 5L), .Label = c(\"Qn1\", \"Qn2\", \"Qn3\", \"Qc1\", \"Qc3\", \"Qc2\", \"Mn3\", \"Mn2\", \"Mn1\", \"Mc2\", \"Mc3\", \"Mc1\"), class = c(\"ordered\", \"factor\")), Type = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c(\"Quebec\", \"Mississippi\"), class = \"factor\"), Treatment = structure(c(2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c(\"nonchilled\", \"chilled\"), class = \"factor\")), .Names = c(\"Plant\", \"Type\", \"Treatment\"), class = \"data.frame\", row.names = 36:42), \"any\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

