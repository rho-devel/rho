expected <- eval(parse(text="c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 3, 4, 5, 7, 8, 9, 10, 11, 11)"));        
test(id=0, code={        
argv <- eval(parse(text="list(1, c(2, 3, 4, 6, 7, 8, 9, 10, 11), c(3, 4, 5, 7, 8, 9, 10, 11), 11L)"));        
do.call(`c`, argv);        
}, o=expected);        

