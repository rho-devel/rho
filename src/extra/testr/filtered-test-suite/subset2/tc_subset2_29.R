expected <- eval(parse(text="structure(1342423171, class = c(\"POSIXct\", \"POSIXt\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(mtime = structure(1342423171, class = c(\"POSIXct\", \"POSIXt\"))), .Names = \"mtime\"), 1L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

