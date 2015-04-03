expected <- eval(parse(text="c(NA, \"green\", \"black\", \"blue\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(NA, \"green\", \"black\", \"blue\"), 4L)"));          
.Internal(rep_len(argv[[1]], argv[[2]]));          
}, o=expected);          

