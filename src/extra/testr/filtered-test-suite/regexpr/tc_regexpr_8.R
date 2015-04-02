expected <- eval(parse(text="structure(integer(0), match.length = integer(0), useBytes = TRUE)"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"\\\\.([[:alnum:]]+)$\", character(0), FALSE, FALSE, FALSE, FALSE)"));           
.Internal(regexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
}, o=expected);           

