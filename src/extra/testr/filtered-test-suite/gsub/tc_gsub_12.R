expected <- eval(parse(text="\"pkgB_.tar.gz\""));     
test(id=0, code={     
argv <- eval(parse(text="list(\"([[:digit:]]+[.-]){1,}[[:digit:]]+\", \"\", \"pkgB_1.0.tar.gz\", FALSE, FALSE, FALSE, FALSE)"));     
.Internal(`gsub`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));     
}, o=expected);     

