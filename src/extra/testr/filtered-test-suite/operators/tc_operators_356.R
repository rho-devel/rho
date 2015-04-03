expected <- eval(parse(text="structure(NA, .Tsp = c(1, 1, 1), .S3Class = \"ts\", class = structure(\"ts\", package = \"methods\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(NA, .Tsp = c(1, 1, 1), .S3Class = \"ts\", class = structure(\"ts\", package = \"methods\")))"));               
do.call(`(`, argv);               
}, o=expected);               

