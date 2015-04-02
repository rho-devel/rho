expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(list(structure(list(structure(\"R Core\", class = \"AsIs\")), row.names = c(NA, -1L), class = \"data.frame\"), structure(list(structure(NA_character_, class = \"AsIs\")), row.names = c(NA, -1L), class = \"data.frame\"), structure(list(structure(NA_character_, class = \"AsIs\")), row.names = c(NA, -1L), class = \"data.frame\"), structure(list(structure(\"An Introduction to R\", class = \"AsIs\")), row.names = c(NA, -1L), class = \"data.frame\"), structure(list(structure(\"Venables & Smith\", class = \"AsIs\")), row.names = c(NA, -1L), class = \"data.frame\")), FALSE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

