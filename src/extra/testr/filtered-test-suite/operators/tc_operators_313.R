expected <- eval(parse(text="structure(numeric(0), .Dim = c(0L, 0L))"));              
test(id=0, code={              
argv <- eval(parse(text="list(numeric(0), structure(numeric(0), .Dim = c(1L, 0L)))"));              
do.call(`%*%`, argv);              
}, o=expected);              

