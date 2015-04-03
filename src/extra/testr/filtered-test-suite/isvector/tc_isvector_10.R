expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(`1` = c(0, 0, 0, 0, 0, 0, 2.96439387504748e-323, 0, 0, 0, 0, 0)), .Names = \"1\"), \"any\")"));                  
.Internal(is.vector(argv[[1]], argv[[2]]));                  
}, o=expected);                  

