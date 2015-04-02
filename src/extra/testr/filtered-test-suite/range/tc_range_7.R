expected <- eval(parse(text="c(8.2, 30.9)"));           
test(id=0, code={           
argv <- eval(parse(text="list(c(8.2, 9.7, 12.25, 16.5, 21.5, 14.5, 20, 23.45, 25.8, 27.3, 22.4, 24.5, 25.95, 27.3, 30.9), numeric(0), NULL, na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

