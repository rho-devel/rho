expected <- eval(parse(text="c(FALSE, FALSE)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(8262, 2889), 1e+05)"));           
do.call(`>`, argv);           
}, o=expected);           

