expected <- eval(parse(text="c(Inf, -Inf)"));           
test(id=0, code={           
argv <- eval(parse(text="list(list(), na.rm = TRUE)"));           
do.call(`range`, argv);           
}, o=expected);           

