expected <- eval(parse(text="NULL"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1395082220.91387, class = c(\"POSIXct\", \"POSIXt\")))"));       
do.call(`dim`, argv);       
}, o=expected);       

