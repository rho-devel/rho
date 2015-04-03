expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(c(20L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 19L, 20L, 20L, 20L, 20L, 19L, 19L, 19L, 20L, 20L, 20L, 19L, 20L, 19L, 19L, 19L, 20L))"));       
do.call(`is.expression`, argv);       
}, o=expected);       

