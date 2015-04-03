expected <- eval(parse(text="list(structure(2L, .Label = c(\"C\", \"D\"), class = \"factor\"), structure(NA_real_, class = c(\"POSIXct\", \"POSIXt\")))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(structure(list(b = structure(2L, .Label = c(\"C\", \"D\"), class = \"factor\")), .Names = \"b\", row.names = 2L, class = \"data.frame\"), structure(list(a = structure(NA_real_, class = c(\"POSIXct\", \"POSIXt\"))), .Names = \"a\", row.names = \"NA\", class = \"data.frame\")), FALSE, FALSE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

