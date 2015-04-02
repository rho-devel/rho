expected <- eval(parse(text="structure(list(structure(list(lower = 13.2743449189798, est. = 24.8054653131966, upper = 46.3534067526313), .Names = c(\"lower\", \"est.\", \"upper\"), row.names = \"reStruct.Rail.sd((Intercept))\", class = \"data.frame\")), .Dim = c(1L, 1L), .Dimnames = list(\"1\", NULL))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(`1` = structure(list(lower = 13.2743449189798, est. = 24.8054653131966, upper = 46.3534067526313), .Names = c(\"lower\", \"est.\", \"upper\"), row.names = \"reStruct.Rail.sd((Intercept))\", class = \"data.frame\")), .Names = \"1\"), c(1L, 1L), list(\"1\", NULL))"));              
.Internal(array(argv[[1]], argv[[2]], argv[[3]]));              
}, o=expected);              

