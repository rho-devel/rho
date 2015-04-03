expected <- eval(parse(text="c(9.18429112061858e-05, 0.0238094009226188, 0.0498038685764186)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(9.18429112061858e-05, 0.0238094009226188, 0.0498038685764186), FALSE, FALSE, NA)"));              
.Internal(unique(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));              
}, o=expected);              

