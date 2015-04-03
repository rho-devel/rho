expected <- eval(parse(text="structure(c(\"142\", \"104\", \" 71\", \"250\"), .Dim = 4L, .Dimnames = structure(list(c(\"(1) Approve STRONGLY\", \"(2) Approve SOMEWHAT\", \"(3) Disapprove SOMEWHAT\", \"(4) Disapprove STRONGLY\")), .Names = \"\"))"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(142L, 104L, 71L, 250L), .Dim = 4L, .Dimnames = structure(list(c(\"(1) Approve STRONGLY\", \"(2) Approve SOMEWHAT\", \"(3) Disapprove SOMEWHAT\", \"(4) Disapprove STRONGLY\")), .Names = \"\")), FALSE, 7L, 0L, NULL, 3L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

