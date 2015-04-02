expected <- eval(parse(text="c(4-5i, 3-5i, 2-5i, 1-5i, 2-5i, 3-5i, 4-5i, 5-5i)"));               
test(id=0, code={               
argv <- eval(parse(text="list(c(4L, 3L, 2L, 1L, 2L, 3L, 4L, 5L), 0+5i)"));               
do.call(`-`, argv);               
}, o=expected);               

