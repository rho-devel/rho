expected <- eval(parse(text="c(1L, 3L)"));           
test(id=0, code={           
argv <- eval(parse(text="list(1:3, finite = TRUE, na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

