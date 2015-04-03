expected <- eval(parse(text="1:6"));   
test(id=0, code={   
argv <- eval(parse(text="list(1:6, value = NULL)"));   
do.call(`attributes<-`, argv);   
}, o=expected);   

