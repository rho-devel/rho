expected <- eval(parse(text="c(0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE))"));    
do.call(`as.integer`, argv);    
}, o=expected);    

