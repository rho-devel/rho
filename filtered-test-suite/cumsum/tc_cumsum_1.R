expected <- eval(parse(text="c(9L, 14L, 27L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(9L, 5L, 13L))"));  
do.call(`cumsum`, argv);  
}, o=expected);  

