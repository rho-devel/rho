expected <- eval(parse(text="NA"));   
test(id=0, code={   
argv <- eval(parse(text="list(NA, value = NULL)"));   
do.call(`attributes<-`, argv);   
}, o=expected);   

