expected <- eval(parse(text="structure(\"data.frame\", package = \"methods\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(), .Names = character(0), row.names = integer(0), .S3Class = \"data.frame\", class = structure(\"data.frame\", package = \"methods\")))"));              
do.call(`class`, argv);              
}, o=expected);              

