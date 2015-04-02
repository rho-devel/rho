expected <- eval(parse(text="structure(list(names = c(\"x\", \"z\"), class = \"data.frame\", row.names = c(NA, 10L)), .Names = c(\"names\", \"class\", \"row.names\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(names = c(\"x\", \"z\")), .Names = \"names\"), structure(list(class = \"data.frame\", row.names = c(NA, 10L)), .Names = c(\"class\", \"row.names\")))"));        
do.call(`c`, argv);        
}, o=expected);        

