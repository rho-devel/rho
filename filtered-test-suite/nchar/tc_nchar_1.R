expected <- eval(parse(text="76L"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"DtTmCl> format(.leap.seconds)         # all 24 leap seconds in your timezone\", \"c\", FALSE)"));     
.Internal(`nchar`(argv[[1]], argv[[2]], argv[[3]]));     
}, o=expected);     

