expected <- eval(parse(text="c(\"trace\", \"fnscale\", \"parscale\", \"ndeps\", \"maxit\", \"abstol\", \"reltol\", \"alpha\", \"beta\", \"gamma\", \"REPORT\", \"type\", \"lmm\", \"factr\", \"pgtol\", \"tmax\", \"temp\")"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(trace = 0, fnscale = 1, parscale = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), ndeps = c(0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001, 0.001), maxit = 100L, abstol = -Inf, reltol = 1.49011611938477e-08, alpha = 1, beta = 0.5, gamma = 2, REPORT = 10, type = 1, lmm = 5, factr = 1e+07, pgtol = 0, tmax = 10, temp = 10), .Names = c(\"trace\", \"fnscale\", \"parscale\", \"ndeps\", \"maxit\", \"abstol\", \"reltol\", \"alpha\", \"beta\", \"gamma\", \"REPORT\", \"type\", \"lmm\", \"factr\", \"pgtol\", \"tmax\", \"temp\")))"));                   
do.call(`names`, argv);                   
}, o=expected);                   

