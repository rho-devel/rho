expected <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(12L, 1L), .Dimnames = structure(list(`  p L s` = c(\". . .\", \"| . .\", \". | .\", \"| | .\", \". . |\", \"| . |\", \". | |\", \"| | |\", \". . ?\", \"| . ?\", \". | ?\", \"| | ?\"), NULL), .Names = c(\"  p L s\", \"\"))))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE), .Dim = c(12L, 1L), .Dimnames = structure(list(`  p L s` = c(\". . .\", \"| . .\", \". | .\", \"| | .\", \". . |\", \"| . |\", \". | |\", \"| | |\", \". . ?\", \"| . ?\", \". | ?\", \"| | ?\"), NULL), .Names = c(\"  p L s\", \"\"))))"));                  
do.call(`list`, argv);                  
}, o=expected);                  

