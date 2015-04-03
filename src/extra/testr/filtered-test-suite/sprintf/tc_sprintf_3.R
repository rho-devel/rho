expected <- eval(parse(text="c(\"min 10-char string '         a'\", \"min 10-char string '       ABC'\", \"min 10-char string 'and an even longer one'\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"min 10-char string '%10s'\", c(\"a\", \"ABC\", \"and an even longer one\"))"));           
.Internal(sprintf(argv[[1]], argv[[2]]));           
}, o=expected);           

