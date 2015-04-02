expected <- eval(parse(text="c(9L, 9L, 8L, 7L, 6L, 5L, 4L, 3L, 2L, 1L)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"9\", \"9\", \"8\", \"7\", \"6\", \"5\", \"4\", \"3\", \"2\", \"1\"))"));     
do.call(`xtfrm`, argv);     
}, o=expected);     

