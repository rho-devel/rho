expected <- eval(parse(text="c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(0L, 1L, 2L, 2L, 2L, 3L, 4L, 5L, 5L, 5L, 6L, 7L, 7L, 7L, 8L, 9L, 9L, 9L, 10L, 11L, 11L, 11L, 12L, 12L, 12L, 13L, 14L, 14L, 14L), 1L)"));             
do.call(`<=`, argv);             
}, o=expected);             

