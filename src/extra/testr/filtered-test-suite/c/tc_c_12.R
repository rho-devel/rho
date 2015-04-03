expected <- eval(parse(text="c(512, 313, 89, 19, 353, 207, 17, 8, 120, 205, 202, 391, 138, 279, 131, 244, 53, 138, 94, 299, 22, 351, 24, 317)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(c(512, 313, 89, 19, 353, 207, 17, 8, 120, 205, 202, 391, 138, 279, 131, 244, 53, 138, 94, 299, 22, 351, 24, 317), .Dim = c(2L, 2L, 6L), .Dimnames = structure(list(Admit = c(\"Admitted\", \"Rejected\"), Gender = c(\"Male\", \"Female\"), Dept = c(\"A\", \"B\", \"C\", \"D\", \"E\", \"F\")), .Names = c(\"Admit\", \"Gender\", \"Dept\")), class = \"table\"))"));        
do.call(`c`, argv);        
}, o=expected);        

