expected <- eval(parse(text="structure(FALSE, .Label = FALSE)"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(FALSE, .Label = FALSE), FALSE)"));   
do.call(`levels<-`, argv);   
}, o=expected);   

