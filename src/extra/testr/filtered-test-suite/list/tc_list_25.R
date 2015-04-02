expected <- eval(parse(text="list(structure(list(x = 1L, y = structure(1L, .Label = c(\"A\", \"D\", \"E\"), class = \"factor\"), z = 6), .Names = c(\"x\", \"y\", \"z\"), row.names = 1L, class = \"data.frame\"), structure(list(), .Names = character(0), row.names = 1L, class = \"data.frame\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(x = 1L, y = structure(1L, .Label = c(\"A\", \"D\", \"E\"), class = \"factor\"), z = 6), .Names = c(\"x\", \"y\", \"z\"), row.names = 1L, class = \"data.frame\"), structure(list(), .Names = character(0), row.names = 1L, class = \"data.frame\"))"));         
do.call(`list`, argv);         
}, o=expected);         

