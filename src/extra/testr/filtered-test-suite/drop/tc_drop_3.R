expected <- eval(parse(text="c(-0.146170181357627, 24.3243243243243, NA, 84.2105263157895, 2.13784643479304)"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(-0.146170181357627, 24.3243243243243, NA, 84.2105263157895, 2.13784643479304), .Dim = c(5L, 1L)))"));             
.Internal(drop(argv[[1]]));             
}, o=expected);             

