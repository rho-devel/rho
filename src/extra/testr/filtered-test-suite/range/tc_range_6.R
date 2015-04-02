expected <- eval(parse(text="c(0, 21)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(1L, 3L, 7L, 14L, 21L, 20L, 19L, 9L, 4L, 2L), 0, na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

