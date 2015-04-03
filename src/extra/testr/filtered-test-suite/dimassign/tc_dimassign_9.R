expected <- eval(parse(text="structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 1L, 2L, 3L, 4L, 5L, 6L, 7L), .Dim = c(3L, 4L, 2L))"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 1L, 2L, 3L, 4L, 5L, 6L, 7L), .Dim = c(3L, 4L, 2L)), value = c(3, 4, 2))"));   
do.call(`dim<-`, argv);   
}, o=expected);   

