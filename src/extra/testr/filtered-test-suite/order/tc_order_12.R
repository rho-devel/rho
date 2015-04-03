expected <- eval(parse(text="1:2"));     
test(id=0, code={     
argv <- eval(parse(text="list(TRUE, FALSE, c(FALSE, FALSE))"));     
.Internal(`order`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

