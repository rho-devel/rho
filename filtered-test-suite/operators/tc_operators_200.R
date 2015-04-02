expected <- eval(parse(text="structure(1395082079.73982, class = c(\"POSIXct\", \"POSIXt\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(1395082079.73982, class = c(\"POSIXct\", \"POSIXt\")))"));     
do.call(`(`, argv);     
}, o=expected);     

