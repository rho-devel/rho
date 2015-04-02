expected <- eval(parse(text="structure(c(794, 150, 86, 570), .Dim = c(2L, 2L), .Dimnames = structure(list(`2nd Survey` = c(\"Approve\", \"Disapprove\"), `1st Survey` = c(\"Approve\", \"Disapprove\")), .Names = c(\"2nd Survey\", \"1st Survey\")))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(794, 86, 150, 570), .Dim = c(2L, 2L), .Dimnames = structure(list(`1st Survey` = c(\"Approve\", \"Disapprove\"), `2nd Survey` = c(\"Approve\", \"Disapprove\")), .Names = c(\"1st Survey\", \"2nd Survey\"))))"));             
.Internal(t.default(argv[[1]]));             
}, o=expected);             

