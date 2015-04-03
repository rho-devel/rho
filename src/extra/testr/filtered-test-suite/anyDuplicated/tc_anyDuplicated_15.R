expected <- eval(parse(text="0L"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(12784, 13149, 13514, 13879, 14245, 14610), FALSE, FALSE)"));     
.Internal(`anyDuplicated`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

