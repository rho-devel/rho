expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(structure(list(structure(1:4, .Label = c(\"0\", \"1\", \"2\", \"3\"), class = \"factor\")), row.names = c(NA, -4L), class = \"data.frame\")), FALSE)"));                 
.Internal(islistfactor(argv[[1]], argv[[2]]));                 
}, o=expected);                 

