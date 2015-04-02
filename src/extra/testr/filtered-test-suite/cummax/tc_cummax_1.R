expected <- eval(parse(text="c(3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(3L, 2L, 1L, 2L, 1L, 0L, 4L, 3L, 2L))"));  
do.call(`cummax`, argv);  
}, o=expected);  

