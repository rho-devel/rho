expected <- eval(parse(text="structure(list(V1 = c(1L, 1L, 2L, 3L), V2 = structure(c(1L, 1L, 2L, 3L), .Label = c(\"A\", \"D\", \"E\"), class = \"factor\"), V3 = c(6, 6, 9, 10), sep = \"\\r\"), .Names = c(\"V1\", \"V2\", \"V3\", \"sep\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(V1 = c(1L, 1L, 2L, 3L), V2 = structure(c(1L, 1L, 2L, 3L), .Label = c(\"A\", \"D\", \"E\"), class = \"factor\"), V3 = c(6, 6, 9, 10)), .Names = c(\"V1\", \"V2\", \"V3\"), row.names = c(NA, 4L), class = \"data.frame\"), sep = \"\\r\")"));        
do.call(`c`, argv);        
}, o=expected);        

