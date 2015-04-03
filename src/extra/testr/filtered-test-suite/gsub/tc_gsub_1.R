expected <- eval(parse(text="\"Date-Time Classes\""));     
test(id=0, code={     
argv <- eval(parse(text="list(\"([[:alnum:]])--([[:alnum:]])\", \"\\\\1-\\\\2\", \"Date-Time Classes\", FALSE, FALSE, FALSE, FALSE)"));     
.Internal(`gsub`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));     
}, o=expected);     

