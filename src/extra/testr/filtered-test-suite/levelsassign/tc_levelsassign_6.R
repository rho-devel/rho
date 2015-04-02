expected <- eval(parse(text="structure(list(), .Label = list())"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(), .Label = list()), list())"));   
do.call(`levels<-`, argv);   
}, o=expected);   

