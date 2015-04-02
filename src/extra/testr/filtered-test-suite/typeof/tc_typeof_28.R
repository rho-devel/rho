expected <- eval(parse(text="\"integer\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(1L, 1L), .Label = \"Ctl\", class = \"factor\"))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

