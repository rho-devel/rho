expected <- eval(parse(text="c(2.44948974278318, 2.23606797749979, 2, 1.73205080756888, 1.4142135623731, 1, 0, NaN, NaN, NaN, NaN)"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(6L, 5L, 4L, 3L, 2L, 1L, 0L, -1L, -2L, -3L, -4L))"));            
do.call(`sqrt`, argv);            
}, o=expected);            

