expected <- eval(parse(text="structure(1:12, .Dim = 3:4)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:12, .Dim = 3:4), value = 3:4)"));   
do.call(`dim<-`, argv);   
}, o=expected);   

