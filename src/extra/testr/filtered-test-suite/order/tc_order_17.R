expected <- eval(parse(text="1:5"));     
test(id=0, code={     
argv <- eval(parse(text="list(TRUE, FALSE, structure(c(1, 2, 2, 2, 2), .Names = c(\"character\", \"vector\", \"data.frameRowLabels\", \"SuperClassMethod\", \"atomicVector\")))"));     
.Internal(`order`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

