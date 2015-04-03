expected <- eval(parse(text="structure(c(1L, 3L), .Label = c(\"A\", \"B\", \"C\"), class = c(\"ordered\", \"factor\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(3L, 2L, 1L), .Label = c(\"A\", \"B\", \"C\"), class = c(\"ordered\", \"factor\")), na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

