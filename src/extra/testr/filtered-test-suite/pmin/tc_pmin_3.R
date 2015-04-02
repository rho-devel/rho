expected <- eval(parse(text="c(1, 1, 1, 1)"));           
test(id=0, code={           
argv <- eval(parse(text="list(FALSE, c(19.7787405591752, 12504507.4953993, 12504507.4953993, 5.96190157728191e+41), 1)"));           
.Internal(pmin(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

