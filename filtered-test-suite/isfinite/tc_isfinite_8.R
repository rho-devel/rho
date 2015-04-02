expected <- eval(parse(text="c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE)"));              
test(id=0, code={              
argv <- eval(parse(text="list(c(96L, 78L, 73L, 91L, 47L, 32L, 20L, 23L, 21L, 24L, 44L, 21L, 28L, 9L, 13L, 46L, 18L, 13L, 24L, 16L, 13L, 23L, 36L, 7L, 14L, 30L, NA, 14L, 18L, 20L))"));              
do.call(`is.finite`, argv);              
}, o=expected);              

