expected <- eval(parse(text="\"faile\""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"façile\"   , \"latin1\", \"ASCII\", \"\", TRUE, FALSE)"));           
.Internal(iconv(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
}, o=expected);           

