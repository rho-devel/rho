expected <- eval(parse(text="81L"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(-Inf, -Inf, 0, 0, 1, 2, Inf, Inf, Inf, -Inf, -Inf, 0, 0.5, 1, 2, Inf, Inf, Inf, -Inf, -Inf, -Inf, 0, 1, 2, 2, Inf, Inf, -Inf, -Inf, -Inf, 0, 0.8, 1.6, Inf, Inf, Inf, -Inf, -Inf, -Inf, 0.5, 1.3, Inf, Inf, Inf, Inf, -Inf, -Inf, -Inf, 0.5, 1.4, Inf, Inf, Inf, Inf, -Inf, -Inf, -Inf, 0.5, 1.2, 1.9, Inf, Inf, Inf, -Inf, -Inf, -Inf, 0.499999999999999, 1.33333333333333, Inf, Inf, Inf, Inf, -Inf, -Inf, -Inf, 0.5, 1.325, Inf, Inf, Inf, Inf), .Dim = c(9L, 9L), .Dimnames = list(c(\"20%\", \"30%\", \"40%\", \"50%\", \"60%\", \"70%\", \"80%\", \"90%\", \"100%\"), NULL)))"));          
do.call(`length`, argv);          
}, o=expected);          

