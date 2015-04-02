expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(-Inf, -Inf, -Inf, -Inf, 0, 1, 2, 3, Inf, Inf, Inf))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

