expected <- eval(parse(text="FALSE"));         
test(id=0, code={         
argv <- eval(parse(text="list(c(-1, 1, -1, 1))"));         
do.call(`is.character`, argv);         
}, o=expected);         

