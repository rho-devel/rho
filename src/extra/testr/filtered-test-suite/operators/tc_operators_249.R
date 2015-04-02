expected <- eval(parse(text="c(FALSE, FALSE, FALSE, FALSE, FALSE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(1, 1, 1, 1, 1), -10)"));  
do.call(`<`, argv);  
}, o=expected);  

