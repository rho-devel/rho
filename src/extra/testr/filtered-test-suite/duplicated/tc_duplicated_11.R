expected <- eval(parse(text="c(FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(-0.838428742794102, 0.838428742794102, 0.838428742794102, 0.838428742794102, -0.838428742794102, -0.838428742794102), .Dim = c(6L, 1L), .Dimnames = list(c(\"1\", \"3\", \"4\", \"5\", \"6\", \"7\"), NULL)), FALSE, FALSE, NA)"));           
.Internal(duplicated(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));           
}, o=expected);           

