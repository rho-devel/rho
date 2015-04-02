expected <- eval(parse(text="structure(c(2.31273022636069, 2.74511961125388, 0, 0.432389384893196), .Dim = c(4L, 1L), .Dimnames = list(c(\"1\", \"6\", \"8\", \"9\"), NULL))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(0.432389384893196, 2.31273022636069, 0, 2.31273022636069, 0.432389384893196, 0), .Names = c(\"1\", \"3\", \"4\", \"5\", \"6\", \"7\")), structure(c(9, 1, 1, 6, 6, 8), .Names = c(\"1\", \"3\", \"4\", \"5\", \"6\", \"7\")), c(1, 6, 8, 9), FALSE, c(\"1\", \"6\", \"8\", \"9\"))"));       
.Internal(rowsum_matrix(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));       
}, o=expected);       

