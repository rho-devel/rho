expected <- eval(parse(text="3L"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(A = c(1, NA, 1), B = c(1.1, NA, 2), C = c(1.1+0i, NA, 3+0i), D = c(NA_integer_, NA_integer_, NA_integer_), E = c(FALSE, NA, TRUE), F = c(\"abc\", NA, \"def\")), .Names = c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\"), class = \"data.frame\", row.names = c(\"1\", \"2\", \"3\")), 2L)"));                 
.Internal(shortRowNames(argv[[1]], argv[[2]]));                 
}, o=expected);                 

