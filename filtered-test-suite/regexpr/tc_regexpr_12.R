expected <- eval(parse(text="structure(-1L, match.length = -1L, useBytes = TRUE)"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"package:\", \"environmental\", FALSE, FALSE, TRUE, FALSE)"));           
.Internal(regexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
}, o=expected);           

