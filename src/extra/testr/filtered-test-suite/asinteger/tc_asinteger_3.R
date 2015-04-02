expected <- eval(parse(text="c(NA, -8L, -2L, -1L, 0L, 0L, 0L, 0L, 0L, 0L)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(-Inf, -8.5, -2.83333333333333, -1.41666666666667, -0.85, -0.566666666666666, -0.404761904761905, -0.303571428571428, -0.236111111111111, -0.188888888888889))"));              
do.call(`as.integer`, argv);              
}, o=expected);              

