expected <- eval(parse(text="c(FALSE, FALSE, FALSE, FALSE, FALSE)"));             
test(id=0, code={             
argv <- eval(parse(text="list(c(17, 45.1, 39.7, 36.5, 43.5), 6)"));             
do.call(`<=`, argv);             
}, o=expected);             

