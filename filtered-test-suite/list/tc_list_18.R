expected <- eval(parse(text="structure(list(date = structure(1065672000, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\")), .Names = \"date\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(date = structure(1065672000, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"\"))"));         
do.call(`list`, argv);         
}, o=expected);         

