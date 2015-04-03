expected <- eval(parse(text="1:3"));   
test(id=0, code={   
argv <- eval(parse(text="list(1:3, value = \"numeric\")"));   
do.call(`class<-`, argv);   
}, o=expected);   

