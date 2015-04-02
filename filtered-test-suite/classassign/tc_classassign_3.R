expected <- eval(parse(text="character(0)"));   
test(id=0, code={   
argv <- eval(parse(text="list(character(0), character(0))"));   
do.call(`class<-`, argv);   
}, o=expected);   

