expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(row.names = character(0), A = numeric(0), B = numeric(0), C = complex(0), D = integer(0), E = logical(0), F = character(0)), .Names = c(\"row.names\", \"A\", \"B\", \"C\", \"D\", \"E\", \"F\")), \"any\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

