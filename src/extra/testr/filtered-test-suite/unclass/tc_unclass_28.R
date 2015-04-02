expected <- eval(parse(text="structure(c(\"0\", \"list\", \"list\"), .Names = c(\"Length\", \"Class\", \"Mode\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(c(\"0\", \"list\", \"list\"), .Names = c(\"Length\", \"Class\", \"Mode\")))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

