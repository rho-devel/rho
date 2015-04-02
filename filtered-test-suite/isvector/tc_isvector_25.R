expected <- eval(parse(text="FALSE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(A = c(1L, NA, 1L), B = c(1.1, NA, 2), C = c(1.1+0i, NA, 3+0i), D = c(NA, NA, NA), E = c(FALSE, NA, TRUE), F = structure(c(1L, NA, 2L), .Label = c(\"abc\", \"def\"), class = \"factor\")), .Names = c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\"), class = \"data.frame\", row.names = c(\"1\", \"2\", \"3\")), \"any\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

