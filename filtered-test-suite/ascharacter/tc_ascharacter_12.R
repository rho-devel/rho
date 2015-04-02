expected <- eval(parse(text="c(\"1\", \"2\", \"3\", \"4\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(1:4, .Dim = c(1L, 4L)))"));        
do.call(`as.character`, argv);        
}, o=expected);        

