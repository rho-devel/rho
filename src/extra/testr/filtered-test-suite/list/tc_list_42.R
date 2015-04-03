expected <- eval(parse(text="list(structure(c(NA, NA, FALSE), .Names = c(\"perm\", \"LDL\", \"super\")))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(c(NA, NA, FALSE), .Names = c(\"perm\", \"LDL\", \"super\")))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

