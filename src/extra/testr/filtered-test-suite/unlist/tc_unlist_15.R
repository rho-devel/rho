expected <- eval(parse(text="list(structure(c(1395082040.29392, 1395082040.29392, 1395082040.29392, 1395082040.29392, 1395082040.29392), class = c(\"AsIs\", \"POSIXct\", \"POSIXt\")))"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(structure(list(structure(c(1395082040.29392, 1395082040.29392, 1395082040.29392, 1395082040.29392, 1395082040.29392), class = c(\"AsIs\", \"POSIXct\", \"POSIXt\"))), row.names = c(NA, -5L), class = \"data.frame\")), FALSE, FALSE)"));       
.Internal(`unlist`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

