expected <- eval(parse(text="c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0)"));        
test(id=0, code={        
argv <- eval(parse(text="list(0, c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE))"));        
do.call(`c`, argv);        
}, o=expected);        

