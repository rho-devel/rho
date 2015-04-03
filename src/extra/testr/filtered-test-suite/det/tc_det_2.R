expected <- eval(parse(text="structure(list(modulus = structure(-Inf, logarithm = TRUE), sign = 1L), .Names = c(\"modulus\", \"sign\"), class = \"det\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE), .Dim = c(2L, 2L), .Dimnames = list(c(\"A\", \"B\"), c(\"A\", \"B\"))), TRUE)"));     
.Internal(det_ge_real(argv[[1]], argv[[2]]));     
}, o=expected);     

