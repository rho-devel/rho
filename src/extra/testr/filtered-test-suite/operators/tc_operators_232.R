expected <- eval(parse(text="structure(1395078479.75887, class = c(\"POSIXct\", \"POSIXt\"))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(1395082079.75887, class = c(\"POSIXct\", \"POSIXt\")), 3600)"));      
do.call(`-`, argv);      
}, o=expected);      

