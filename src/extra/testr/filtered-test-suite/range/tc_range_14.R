expected <- eval(parse(text="c(14.43333, 65.7667)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(14.43333, 65.7667), finite = TRUE, na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

