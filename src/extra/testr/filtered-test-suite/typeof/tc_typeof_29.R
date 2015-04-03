expected <- eval(parse(text="\"integer\""));         
test(id=0, code={         
argv <- eval(parse(text="list(c(1L, NA, 1L))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

