expected <- eval(parse(text="c(\"registered S3method for $\", \"registered S3method for $\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(1L, 1L), .Label = \"registered S3method for $\", class = \"factor\"), \"any\")"));               
.Internal(as.vector(argv[[1]], argv[[2]]));               
}, o=expected);               

