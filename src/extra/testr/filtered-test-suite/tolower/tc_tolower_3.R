expected <- eval(parse(text="c(\"title\", \"author\", \"year\", \"note\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(\"title\", \"author\", \"year\", \"note\"))"));       
.Internal(tolower(argv[[1]]));       
}, o=expected);       

