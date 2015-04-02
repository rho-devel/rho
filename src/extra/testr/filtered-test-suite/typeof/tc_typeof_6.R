expected <- eval(parse(text="\"double\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(-0.0529307911108286, -0.200175675120066), .Names = c(\"(Intercept)\", \"xTRUE\")))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

