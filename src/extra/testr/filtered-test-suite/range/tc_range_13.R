expected <- eval(parse(text="c(NA_real_, NA_real_)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(NA, 1, 2, 3, -Inf, NaN, Inf), na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

