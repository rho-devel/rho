expected <- eval(parse(text="FALSE"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(logical(0), .Dim = c(0L, 0L)))"));             
do.call(`any`, argv);             
}, o=expected);             

