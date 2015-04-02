expected <- eval(parse(text="1L"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 1L), .Names = c(\"vector\", \"data.frameRowLabels\", \"SuperClassMethod\", \"atomicVector\")), FALSE, FALSE, NA)"));      
.Internal(`unique`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));      
}, o=expected);      

