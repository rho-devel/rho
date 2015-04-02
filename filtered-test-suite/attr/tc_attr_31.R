expected <- eval(parse(text="structure(200171400, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\")"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(1:10, date = structure(200171400, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"), class = \"stamped\"), \"date\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

