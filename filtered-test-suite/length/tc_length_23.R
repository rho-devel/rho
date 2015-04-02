expected <- eval(parse(text="3L"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(names = character(0), row.names = integer(0), class = \"data.frame\"), .Names = c(\"names\", \"row.names\", \"class\")))"));          
do.call(`length`, argv);          
}, o=expected);          

