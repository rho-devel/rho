expected <- eval(parse(text="integer(0)"));            
test(id=0, code={            
argv <- eval(parse(text="list(character(0), c(\"labels\", \"col\", \"alpha\", \"adj\", \"cex\", \"lineheight\", \"font\"), NA_integer_, TRUE)"));            
.Internal(pmatch(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));            
}, o=expected);            

