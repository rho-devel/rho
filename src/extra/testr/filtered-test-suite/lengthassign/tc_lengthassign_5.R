expected <- eval(parse(text="integer(0)"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 0L, 1L, 2L, 3L, 4L, 5L, 6L), value = 0L)"));  
do.call(`length<-`, argv);  
}, o=expected);  

