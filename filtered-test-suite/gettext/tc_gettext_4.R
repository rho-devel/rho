expected <- eval(parse(text="c(\"/\", \" not meaningful for factors\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(NULL, c(\"/\", \" not meaningful for factors\"))"));         
.Internal(gettext(argv[[1]], argv[[2]]));         
}, o=expected);         

