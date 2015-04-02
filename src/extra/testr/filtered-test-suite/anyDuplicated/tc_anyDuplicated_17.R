expected <- eval(parse(text="2L"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(1L, 1L, 1L, 1L, 1L), FALSE, FALSE)"));     
.Internal(`anyDuplicated`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

