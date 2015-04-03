expected <- eval(parse(text="structure(c(1208822400, 1209168000), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"GMT\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(1208822400, 1209168000, 1208822400, 1209168000), class = c(\"POSIXct\", \"POSIXt\"), tzone = \"GMT\"), na.rm = TRUE)"));    
do.call(`range`, argv);    
}, o=expected);    

