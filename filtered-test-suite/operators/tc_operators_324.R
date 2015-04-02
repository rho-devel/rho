expected <- eval(parse(text="c(NA, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)"));          
test(id=0, code={          
argv <- eval(parse(text="list(TRUE, 0:10)"));          
do.call(`%/%`, argv);          
}, o=expected);          

