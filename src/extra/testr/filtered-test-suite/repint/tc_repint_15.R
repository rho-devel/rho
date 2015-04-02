expected <- eval(parse(text="c(\"A\", \"A\", \"B\")"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(\"A\", \"B\"), structure(list(A = 2L, B = 1L), .Names = c(\"A\", \"B\")))"));            
.Internal(rep.int(argv[[1]], argv[[2]]));            
}, o=expected);            

