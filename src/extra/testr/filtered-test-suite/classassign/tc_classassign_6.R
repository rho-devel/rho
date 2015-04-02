expected <- eval(parse(text="structure(list(par = 5.5, loglik = 0.970661978016996), .Names = c(\"par\", \"loglik\"), class = \"pfit\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(par = 5.5, loglik = 0.970661978016996), .Names = c(\"par\", \"loglik\"), class = \"pfit\"), value = \"pfit\")"));   
do.call(`class<-`, argv);   
}, o=expected);   

