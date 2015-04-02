expected <- eval(parse(text="structure(1, class = \"bar\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(1, class = \"bar\"), value = \"bar\")"));   
do.call(`class<-`, argv);   
}, o=expected);   

