expected <- eval(parse(text="c(NA, 1L, 2L)"));                
test(id=0, code={                
argv <- eval(parse(text="list(c(\"dMatrix\", \"nonStructure\", \"structure\"), c(\"nonStructure\", \"structure\"), NA_integer_, NULL)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

