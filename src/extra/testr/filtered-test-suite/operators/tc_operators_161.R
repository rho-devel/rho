expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, NA, TRUE, TRUE, TRUE, TRUE)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(1, 2, 3, 4, 5, 6, 7, NA, 9, 10, 11, 12), 1)"));          
do.call(`>=`, argv);          
}, o=expected);          

