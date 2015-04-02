expected <- eval(parse(text="NA_real_"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(NA, 1, 2, 3, -Inf, NaN, Inf))"));              
do.call(`max`, argv);              
}, o=expected);              

