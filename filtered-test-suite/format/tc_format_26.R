expected <- eval(parse(text="structure(c(\"  0\", \"  5\", \"118\", \" 57\", \"  0\", \"  1\", \"  4\", \"140\", \"  0\", \" 11\", \"154\", \" 14\", \"  0\", \" 13\", \" 13\", \" 80\", \" 35\", \" 13\", \"387\", \" 75\", \" 17\", \" 14\", \" 89\", \" 76\", \"  0\", \"  0\", \"670\", \"192\", \"  0\", \"  0\", \"  3\", \" 20\"), .Dim = c(1L, 32L))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(0, 5, 118, 57, 0, 1, 4, 140, 0, 11, 154, 14, 0, 13, 13, 80, 35, 13, 387, 75, 17, 14, 89, 76, 0, 0, 670, 192, 0, 0, 3, 20), .Dim = c(1L, 32L), row.vars = structure(list(), .Names = character(0)), col.vars = structure(list(Class = c(\"1st\", \"2nd\", \"3rd\", \"Crew\"), Sex = c(\"Male\", \"Female\"), Age = c(\"Child\", \"Adult\"), Survived = c(\"No\", \"Yes\")), .Names = c(\"Class\", \"Sex\", \"Age\", \"Survived\"))), FALSE, 7L, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

