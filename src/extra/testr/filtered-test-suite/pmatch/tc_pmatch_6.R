expected <- eval(parse(text="c(3L, 1L, 2L, NA, NA)"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(\"alpha\", \"col\", \"border\", \"lty\", \"lwd\"), c(\"col\", \"border\", \"alpha\", \"size\", \"height\", \"angle\", \"density\"), NA_integer_, TRUE)"));            
.Internal(pmatch(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));            
}, o=expected);            

