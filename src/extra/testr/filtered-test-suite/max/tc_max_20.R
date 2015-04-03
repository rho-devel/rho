expected <- eval(parse(text="structure(1209168000, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"GMT\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(1208822400, 1209168000), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"GMT\"), na.rm = FALSE)"));      
do.call(`max`, argv);      
}, o=expected);      

