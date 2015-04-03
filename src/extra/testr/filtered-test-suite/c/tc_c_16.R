expected <- eval(parse(text="structure(list(other = structure(1:3, .Label = c(\"A\", \"B\", \"C\"), class = \"factor\")), .Names = \"other\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(NULL, structure(list(other = structure(1:3, .Label = c(\"A\", \"B\", \"C\"), class = \"factor\")), .Names = \"other\"))"));        
do.call(`c`, argv);        
}, o=expected);        

