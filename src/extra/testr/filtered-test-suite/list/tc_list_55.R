expected <- eval(parse(text="structure(list(class = \"data.frame\", row.names = c(NA, 32L)), .Names = c(\"class\", \"row.names\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(class = \"data.frame\", row.names = c(NA, 32L))"));         
do.call(`list`, argv);         
}, o=expected);         

