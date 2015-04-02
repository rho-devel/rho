expected <- eval(parse(text="0L"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(12784, 12874, 12965, 13057, 13149, 13239, 13330, 13422, 13514, 13604, 13695, 13787, 13879, 13970, 14061, 14153, 14245, 14335), FALSE, FALSE)"));     
.Internal(`anyDuplicated`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

