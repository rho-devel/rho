expected <- eval(parse(text="FALSE"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE))"));     
do.call(`all`, argv);     
}, o=expected);     

