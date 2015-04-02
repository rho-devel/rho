expected <- eval(parse(text="0L"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"me\", c(\"mean\", \"median\", \"mode\"), NA_integer_)"));     
.Internal(charmatch(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

