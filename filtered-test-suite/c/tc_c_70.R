expected <- eval(parse(text="structure(list(Topic = character(0), File = character(0), sep = \"\\r\"), .Names = c(\"Topic\", \"File\", \"sep\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(Topic = character(0), File = character(0)), .Names = c(\"Topic\", \"File\"), class = \"data.frame\", row.names = integer(0)), sep = \"\\r\")"));                  
do.call(`c`, argv);                  
}, o=expected);                  

