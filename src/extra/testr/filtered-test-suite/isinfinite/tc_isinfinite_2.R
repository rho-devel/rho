expected <- eval(parse(text="c(TRUE, TRUE)"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(Inf, -Inf))"));       
do.call(`is.infinite`, argv);       
}, o=expected);       

