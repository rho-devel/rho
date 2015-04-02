expected <- eval(parse(text="structure(FALSE, class = \"FALSE\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(FALSE, class = \"FALSE\"), FALSE)"));   
do.call(`class<-`, argv);   
}, o=expected);   

