expected <- eval(parse(text="c(-2.12168716972669e-05, 7.51519194600216e-05, -6.21732236176711e-06)"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(-2.12168716972669e-05, 7.51519194600216e-05, -6.21732236176711e-06), .Dim = c(3L, 1L)))"));             
.Internal(drop(argv[[1]]));             
}, o=expected);             

