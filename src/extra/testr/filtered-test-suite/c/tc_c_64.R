expected <- eval(parse(text="structure(c(1386393974.25184, 1386393974.25184), class = c(\"POSIXct\", \"POSIXt\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(1386393974.25184, class = c(\"POSIXct\", \"POSIXt\")), structure(1386393974.25184, class = c(\"POSIXct\", \"POSIXt\")))"));                  
do.call(`c`, argv);                  
}, o=expected);                  

