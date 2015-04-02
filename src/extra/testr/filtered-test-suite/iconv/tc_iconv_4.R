expected <- eval(parse(text="c(\"\", \"Compute a Survival Curve for Censored Data\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(\"\", \"Compute a Survival Curve for Censored Data\"), \"UTF-8\", \"\", \"byte\", FALSE, FALSE)"));           
.Internal(iconv(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));           
}, o=expected);           

