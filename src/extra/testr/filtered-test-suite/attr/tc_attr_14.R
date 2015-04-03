expected <- eval(parse(text="c(\"no\", \"yes\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(1L, 1L, 1L, 2L, 1L), .Label = c(\"no\", \"yes\"), class = \"factor\"), \"levels\")"));        
do.call(`attr`, argv);        
}, o=expected);        

