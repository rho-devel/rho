expected <- eval(parse(text="c(1, 31)"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(1, 13, 31), .Dim = 3L, .Dimnames = list(c(\"1st\", \"2nd\", \"3rd\"))), finite = TRUE, na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

