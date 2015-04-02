expected <- eval(parse(text="TRUE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(1L, .Label = c(\"A\", \"B\", \"C\"), class = c(\"ordered\", \"factor\")), structure(3L, .Label = c(\"A\", \"B\", \"C\"), class = c(\"ordered\", \"factor\")))"));          
do.call(`<`, argv);          
}, o=expected);          

