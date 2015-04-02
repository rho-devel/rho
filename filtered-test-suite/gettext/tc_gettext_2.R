expected <- eval(parse(text="\"\""));         
test(id=0, code={         
argv <- eval(parse(text="list(NULL, \"\")"));         
.Internal(gettext(argv[[1]], argv[[2]]));         
}, o=expected);         

