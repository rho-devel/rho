expected <- eval(parse(text="structure(c(0, 2, 1, 1, 0, 2), .Dim = c(6L, 1L), .Dimnames = structure(list(`  p L s` = c(\". . .\", \". | .\", \". . |\", \". | |\", \". . ?\", \". | ?\"), NULL), .Names = c(\"  p L s\", \"\")))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0), .Dim = c(6L, 3L), .Dimnames = structure(list(`  p L s` = c(\". . .\", \". | .\", \". . |\", \". | |\", \". . ?\", \". | ?\"), c(\"perm\", \"LDL\", \"super\")), .Names = c(\"  p L s\", \"\"))), c(4, 2, 1))"));              
do.call(`%*%`, argv);              
}, o=expected);              

