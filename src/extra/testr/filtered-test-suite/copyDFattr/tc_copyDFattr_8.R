expected <- eval(parse(text="structure(list(File = NULL, Title = NULL, PDF = NULL, Depends = NULL, Keywords = NULL), .Names = c(\"File\", \"Title\", \"PDF\", \"Depends\", \"Keywords\"), row.names = integer(0), class = \"data.frame\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(File = character(0), Title = character(0), PDF = character(0), Depends = list(), Keywords = list()), .Names = c(\"File\", \"Title\", \"PDF\", \"Depends\", \"Keywords\"), row.names = integer(0), class = \"data.frame\"), structure(list(File = NULL, Title = NULL, PDF = NULL, Depends = NULL, Keywords = NULL), .Names = c(\"File\", \"Title\", \"PDF\", \"Depends\", \"Keywords\"), row.names = integer(0), class = \"data.frame\"))"));              
.Internal(copyDFattr(argv[[1]], argv[[2]]));              
}, o=expected);              

