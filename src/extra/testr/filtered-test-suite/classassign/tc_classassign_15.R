expected <- eval(parse(text="structure(1:6, class = \"A\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1:6, class = \"A\"), value = \"A\")"));   
do.call(`class<-`, argv);   
}, o=expected);   

