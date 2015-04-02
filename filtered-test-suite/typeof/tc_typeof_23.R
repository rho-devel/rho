expected <- eval(parse(text="\"double\""));         
test(id=0, code={         
argv <- eval(parse(text="list(2.22044604925031e-16)"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

