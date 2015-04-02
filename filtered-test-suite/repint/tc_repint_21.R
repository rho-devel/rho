expected <- eval(parse(text="c(\"C\", \"A\", \"B\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"C\", \"A\", \"B\"), structure(list(C = 1L, A = 1L, B = 1L), .Names = c(\"C\", \"A\", \"B\")))"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

