expected <- eval(parse(text="integer(0)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(Topic = character(0), File = character(0), Title = character(0), Internal = character(0)), .Names = c(\"Topic\", \"File\", \"Title\", \"Internal\"), row.names = integer(0), class = \"data.frame\"), 0L)"));                 
.Internal(shortRowNames(argv[[1]], argv[[2]]));                 
}, o=expected);                 

