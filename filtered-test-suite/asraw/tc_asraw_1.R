expected <- eval(parse(text="raw(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(numeric(0), .Dim = c(0L, 0L)))"));   
do.call(`as.raw`, argv);   
}, o=expected);   

