expected <- eval(parse(text="structure(c(1012633320L, 1012633620L), class = c(\"POSIXct\", \"POSIXt\"))"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(c(1012633320L, 1012633620L), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), na.rm = TRUE)"));           
do.call(`range`, argv);           
}, o=expected);           

