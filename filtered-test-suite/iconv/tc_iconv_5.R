expected <- eval(parse(text="character(0)"));           
test(id=0, code={           
argv <- eval(parse(text="list(character(0), \"latin1\", \"ASCII\", NA_character_, TRUE, FALSE)"));           
.Internal(iconv(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
}, o=expected);           

