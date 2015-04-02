expected <- eval(parse(text="c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, NA, FALSE, FALSE, FALSE, FALSE)"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(1, 2, 3, 4, 5, 6, 7, NA, 9, 10, 11, 12), 2)"));          
do.call(`<`, argv);          
}, o=expected);          

