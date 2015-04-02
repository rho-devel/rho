expected <- eval(parse(text="\"2014-03-17 14:47:20\""));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(1395082040.29392, class = c(\"POSIXct\", \"POSIXt\")))"));        
do.call(`as.character`, argv);        
}, o=expected);        

