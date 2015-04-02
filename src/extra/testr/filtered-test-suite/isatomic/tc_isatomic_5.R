expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(2.5, 6, 6, 7.5, 8, 8, 16, 6, 5, 6, 28, 5, 9.5, 6, 4.5, 10, 14, 3, 4.5, 5.5, 3, 3.5, 6, 2, 3, 4, 6, 5, 6.5, 5, 10, 6, 18, 4.5, 20))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

