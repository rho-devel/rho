expected <- eval(parse(text="structure(c(0, 1, 1, 1, 1, 0), .Names = c(\"mean1\", \"mean2\", \"vcov1\", \"vcov2\", \"vcov3\", \"vcov4\"), skeleton = structure(list(mean = c(0, 1), vcov = structure(c(1, 1, 1, 0), .Dim = c(2L, 2L))), .Names = c(\"mean\", \"vcov\"), class = c(\"relistable\", \"list\")))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(mean = c(0, 1), vcov = structure(c(1, 1, 1, 0), .Dim = c(2L, 2L))), .Names = c(\"mean\", \"vcov\"), class = c(\"relistable\", \"list\")), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

