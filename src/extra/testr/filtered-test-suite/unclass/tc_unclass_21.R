expected <- eval(parse(text="structure(c(325, 285, 706, 885), .Dim = c(1L, 4L), row.vars = structure(list(), .Names = character(0)), col.vars = structure(list(Class = c(\"1st\", \"2nd\", \"3rd\", \"Crew\")), .Names = \"Class\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(c(325, 285, 706, 885), .Dim = c(1L, 4L), row.vars = structure(list(), .Names = character(0)), col.vars = structure(list(Class = c(\"1st\", \"2nd\", \"3rd\", \"Crew\")), .Names = \"Class\")))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

