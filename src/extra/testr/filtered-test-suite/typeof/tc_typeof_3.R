expected <- eval(parse(text="\"double\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(NA, 9, 3, 3), .Names = c(\"<none>\", \"Hair:Eye\", \"Hair:Sex\", \"Eye:Sex\")))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

