expected <- eval(parse(text="c(8L, 10L, 22L, 28L, 32L, 37L, 50L)"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(8L, 2L, 12L, 6L, 4L, 5L, 13L))"));             
do.call(`cumsum`, argv);             
}, o=expected);             

