expected <- eval(parse(text="list(structure(1386392034.50546, class = c(\"POSIXct\", \"POSIXt\")))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(1386392034.50546, class = c(\"POSIXct\", \"POSIXt\")))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

