expected <- eval(parse(text="c(0.518729163482892, 0.869532737559893)"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(0.57459950307683, 1.3311607364495))"));   
do.call(`tanh`, argv);   
}, o=expected);   

