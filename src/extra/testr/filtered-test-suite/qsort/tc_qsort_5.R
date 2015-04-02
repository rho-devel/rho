expected <- eval(parse(text="structure(list(x = c(1L, 3L, 7L, 8L, 11L, 12L, 13L, 19L, 25L), ix = c(1L, 8L, 2L, 9L, 3L, 4L, 5L, 6L, 7L)), .Names = c(\"x\", \"ix\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(1L, 7L, 11L, 12L, 13L, 19L, 25L, 3L, 8L), TRUE)"));        
.Internal(qsort(argv[[1]], argv[[2]]));        
}, o=expected);        

