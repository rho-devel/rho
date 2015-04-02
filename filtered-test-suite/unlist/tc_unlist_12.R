expected <- eval(parse(text="structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Names = c(\"vector\", \"atomicVector\", \"index\", \"numIndex\", \"numLike\", \"number\", \"replValue\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(vector = TRUE, atomicVector = TRUE, index = TRUE, numIndex = TRUE, numLike = TRUE, number = TRUE, replValue = TRUE), .Names = c(\"vector\", \"atomicVector\", \"index\", \"numIndex\", \"numLike\", \"number\", \"replValue\")), FALSE, TRUE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

