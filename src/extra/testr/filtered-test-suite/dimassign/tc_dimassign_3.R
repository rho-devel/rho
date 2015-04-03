expected <- eval(parse(text="structure(1:12, .Dim = 12L)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:12, .Dim = 12L), value = 12L)"));   
do.call(`dim<-`, argv);   
}, o=expected);   

