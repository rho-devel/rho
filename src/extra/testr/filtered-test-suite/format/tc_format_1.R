expected <- eval(parse(text="structure(c(\" 0.0\", \"72.7\", \"56.4\", \"72.7\", \" 0.0\", \"63.3\", \"56.4\", \"63.3\", \" 0.0\"), .Dim = c(3L, 3L), .Dimnames = list(c(\"Girth\", \"Height\", \"Volume\"), c(\"Girth\", \"Height\", \"Volume\")))"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(0, 72.7, 56.4, 72.7, 0, 63.3, 56.4, 63.3, 0), .Dim = c(3L, 3L), .Dimnames = list(c(\"Girth\", \"Height\", \"Volume\"), c(\"Girth\", \"Height\", \"Volume\"))), FALSE, 7L, 0L, NULL, 3L, TRUE, NA)"));      
.Internal(`format`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));      
}, o=expected);      

