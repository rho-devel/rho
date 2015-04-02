expected <- eval(parse(text="structure(c(1208822400, 1209168000), class = c(\"POSIXct\", \"POSIXt\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(1208822400, class = c(\"POSIXct\", \"POSIXt\")), structure(1209168000, class = c(\"POSIXct\", \"POSIXt\")))"));        
do.call(`c`, argv);        
}, o=expected);        

