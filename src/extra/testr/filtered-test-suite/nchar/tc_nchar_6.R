expected <- eval(parse(text="c(6L, 6L, 6L, 6L, 7L, 7L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"0.0470\", \"0.0130\", \"0.0020\", \"0.0001\", \"2.3e-05\", \"4.5e-06\"), \"w\", FALSE)"));     
.Internal(`nchar`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

