expected <- eval(parse(text="structure(list(z = structure(c(1395082040.29392, 1395082040.29392, 1395082040.29392, 1395082040.29392, 1395082040.29392), class = c(\"AsIs\", \"POSIXct\", \"POSIXt\"))), .Names = \"z\", row.names = c(NA, -5L), class = \"data.frame\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(list(z = structure(c(1395082040.29392, 1395082040.29392, 1395082040.29392, 1395082040.29392, 1395082040.29392), class = c(\"AsIs\", \"POSIXct\", \"POSIXt\"))), .Names = \"z\", row.names = c(NA, -5L), class = \"data.frame\"))"));      
do.call(`invisible`, argv);      
}, o=expected);      

