expected <- eval(parse(text="structure(list(names = character(0), row.names = integer(0), .S3Class = \"data.frame\", extra = character(0)), .Names = c(\"names\", \"row.names\", \".S3Class\", \"extra\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(), .Names = character(0), row.names = integer(0), .S3Class = \"data.frame\", extra = character(0)))"));               
do.call(`attributes`, argv);               
}, o=expected);               

