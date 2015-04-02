expected <- eval(parse(text="\"logical\""));         
test(id=0, code={         
argv <- eval(parse(text="list(c(NA, NA, NA))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

