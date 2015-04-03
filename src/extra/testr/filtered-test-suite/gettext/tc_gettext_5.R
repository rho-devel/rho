expected <- eval(parse(text="character(0)"));         
test(id=0, code={         
argv <- eval(parse(text="list(NULL, character(0))"));         
.Internal(gettext(argv[[1]], argv[[2]]));         
}, o=expected);         

