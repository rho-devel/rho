expected <- eval(parse(text="c(FALSE, FALSE, FALSE, FALSE)"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(14, 2, 2, -7), c(14, 2, 2, -7))"));         
do.call(`!=`, argv);         
}, o=expected);         

