expected <- eval(parse(text="structure(list(F = c(326L, 688L, 343L, 98L), R = c(38L, 116L, 84L, 48L), M = c(241L, 584L, 909L, 403L), D = c(110L, 188L, 412L, 681L), B = c(3L, 4L, 26L, 85L)), .Names = c(\"F\", \"R\", \"M\", \"D\", \"B\"), class = \"data.frame\", row.names = c(\"blue\", \"light\", \"medium\", \"dark\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(fair = c(326L, 688L, 343L, 98L), red = c(38L, 116L, 84L, 48L), medium = c(241L, 584L, 909L, 403L), dark = c(110L, 188L, 412L, 681L), black = c(3L, 4L, 26L, 85L)), .Names = c(\"fair\", \"red\", \"medium\", \"dark\", \"black\"), class = \"data.frame\", row.names = c(\"blue\", \"light\", \"medium\", \"dark\")), value = list(c(\"blue\", \"light\", \"medium\", \"dark\"), c(\"F\", \"R\", \"M\", \"D\", \"B\")))"));        
do.call(`dimnames<-`, argv);        
}, o=expected);        

