expected <- eval(parse(text="c(\"A\", \"B\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(2:3, .Label = c(\"C\", \"A\", \"B\"), class = \"factor\"))"));        
do.call(`as.character`, argv);        
}, o=expected);        

