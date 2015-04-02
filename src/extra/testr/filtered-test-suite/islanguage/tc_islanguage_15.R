expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(3L, 3L, NA, 3L, 4L, 3L, NA, 3L, 3L, 3L, 3L, 2L, 3L, 3L, 4L, 3L, 2L, 2L, 3L, 5L, 2L, 2L, 2L, 4L, 3L, 3L, 3L, 3L, 4L, 4L, 3L, 3L, 4L, 3L, 4L, 3L, 3L, 4L, 3L, 1L, 3L, 3L, 5L, 3L, NA, 2L, 4L, 1L, 3L, 3L, NA, 2L, 5L, 3L, 4L, 4L, 5L, 4L, 4L, 3L, 5L, 4L, 4L, NA, 3L, 5L, 5L, 5L, 5L, 4L, 5L, 4L, 4L, 5L))"));            
do.call(`is.language`, argv);            
}, o=expected);            

