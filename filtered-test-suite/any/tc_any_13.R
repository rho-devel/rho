expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(FALSE, .Dim = c(1L, 1L)))"));             
do.call(`any`, argv);             
}, o=expected);             

