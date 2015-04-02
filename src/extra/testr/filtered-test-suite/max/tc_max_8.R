expected <- eval(parse(text="8L"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(6L, 3L, 8L, 4L, 4L, 5L, 7L, 8L, 5L), .Dim = 9L, .Dimnames = structure(list(state.division = c(\"New England\", \"Middle Atlantic\", \"South Atlantic\", \"East South Central\", \"West South Central\", \"East North Central\", \"West North Central\", \"Mountain\", \"Pacific\")), .Names = \"state.division\"), class = \"table\"))"));              
do.call(`max`, argv);              
}, o=expected);              

