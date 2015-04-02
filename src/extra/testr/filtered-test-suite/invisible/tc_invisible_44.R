expected <- eval(parse(text="structure(list(TEST = structure(c(1L, 2L, 6L, 3L, 4L, 5L, 10L, 11L, 9L, 7L, 8L), .Label = c(\"1\", \"2\", \"4\", \"5\", \"\\\\040\", \"\\\\b\", \"\\\\n\", \"\\\\r\", \"\\\\t\", \"\\\\x20\", \"c:\\\\spencer\\\\tests\"), class = \"factor\")), .Names = \"TEST\", class = \"data.frame\", row.names = c(NA, -11L))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(TEST = structure(c(1L, 2L, 6L, 3L, 4L, 5L, 10L, 11L, 9L, 7L, 8L), .Label = c(\"1\", \"2\", \"4\", \"5\", \"\\\\040\", \"\\\\b\", \"\\\\n\", \"\\\\r\", \"\\\\t\", \"\\\\x20\", \"c:\\\\spencer\\\\tests\"), class = \"factor\")), .Names = \"TEST\", class = \"data.frame\", row.names = c(NA, -11L)))"));      
do.call(`invisible`, argv);      
}, o=expected);      

