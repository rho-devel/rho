expected <- eval(parse(text="structure(list(names = c(\"freq\", \"score\"), class = \"data.frame\", row.names = integer(0)), .Names = c(\"names\", \"class\", \"row.names\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(names = c(\"freq\", \"score\")), .Names = \"names\"), structure(list(class = \"data.frame\", row.names = integer(0)), .Names = c(\"class\", \"row.names\")))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

