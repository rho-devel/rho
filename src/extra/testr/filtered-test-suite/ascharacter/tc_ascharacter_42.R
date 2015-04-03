expected <- eval(parse(text="c(\"2005-01-01\", \"2008-01-01\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(12784, 13879), class = \"Date\"))"));        
do.call(`as.character`, argv);        
}, o=expected);        

