expected <- eval(parse(text="structure(list(latin1 = 0L, utf8 = 0L, bytes = 0L, unknown = structure(character(0), .Dim = c(0L, 2L), .Dimnames = list(NULL, c(\"non_ASCII\", \"where\")))), .Names = c(\"latin1\", \"utf8\", \"bytes\", \"unknown\"), class = \"check_package_datasets\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(latin1 = 0L, utf8 = 0L, bytes = 0L, unknown = structure(character(0), .Dim = c(0L, 2L), .Dimnames = list(NULL, c(\"non_ASCII\", \"where\")))), .Names = c(\"latin1\", \"utf8\", \"bytes\", \"unknown\"), class = \"check_package_datasets\"))"));             
do.call(`invisible`, argv);             
}, o=expected);             

