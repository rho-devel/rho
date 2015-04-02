expected <- eval(parse(text="NULL"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(NA, NA, NA, NA, NA, 1L, 2L), .Label = c(\"Ripley\", \"Venables & Smith\"), class = \"factor\"))"));         
do.call(`names`, argv);         
}, o=expected);         

