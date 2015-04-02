expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(2L, 1L, NA))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

