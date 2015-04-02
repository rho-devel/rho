expected <- eval(parse(text="\"utilities \""));     
test(id=0, code={     
argv <- eval(parse(text="list(\"^\\\\s+\", \"\", \" utilities \", FALSE, TRUE, FALSE, TRUE)"));     
.Internal(`gsub`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));     
}, o=expected);     

