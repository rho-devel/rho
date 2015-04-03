expected <- eval(parse(text="structure(c(16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16, 16), class = \"table\", .Dim = 27L, .Dimnames = structure(list(groups = c(\"M01\", \"M02\", \"M03\", \"M04\", \"M05\", \"M06\", \"M07\", \"M08\", \"M09\", \"M10\", \"M11\", \"M12\", \"M13\", \"M14\", \"M15\", \"M16\", \"F01\", \"F02\", \"F03\", \"F04\", \"F05\", \"F06\", \"F07\", \"F08\", \"F09\", \"F10\", \"F11\")), .Names = \"groups\"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L), .Dim = 27L, .Dimnames = structure(list(groups = c(\"M01\", \"M02\", \"M03\", \"M04\", \"M05\", \"M06\", \"M07\", \"M08\", \"M09\", \"M10\", \"M11\", \"M12\", \"M13\", \"M14\", \"M15\", \"M16\", \"F01\", \"F02\", \"F03\", \"F04\", \"F05\", \"F06\", \"F07\", \"F08\", \"F09\", \"F10\", \"F11\")), .Names = \"groups\"), class = \"table\"), 2)"));              
do.call(`^`, argv);              
}, o=expected);              

