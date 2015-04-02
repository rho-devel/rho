expected <- eval(parse(text="c(TRUE, TRUE, TRUE, NA, FALSE, FALSE, TRUE, NA)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(1, 2, 3, NA, -1, 0, 1, NA), 0)"));           
do.call(`>`, argv);           
}, o=expected);           

