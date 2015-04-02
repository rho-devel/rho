expected <- eval(parse(text="1:2"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"m\", \"f\"), c(\"male\", \"female\"), NA_integer_)"));     
.Internal(charmatch(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

