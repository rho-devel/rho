expected <- eval(parse(text="structure(c(\"mode\", \"length\", \"x\", \"mode\", \"x\", \"mode\"), .Dim = 2:3)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(c(\"mode\", \"length\", \"x\", \"mode\", \"x\", \"mode\"), .Dim = 2:3), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

