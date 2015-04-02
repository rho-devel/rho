expected <- eval(parse(text="structure(c(NA_real_, NA_real_, NA_real_), .Names = c(\"sec\", \"min\", \"hour\"))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(sec = NA_real_, min = NA_integer_, hour = NA_integer_), .Names = c(\"sec\", \"min\", \"hour\")), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

