expected <- eval(parse(text="c(\"  \\036 The ‘internal’ graphics device invoked by .Call(\\\"R_GD_nullDevice\\\",\", \"    package = \\\"grDevices\\\") has been removed: use pdf(file = NULL)\", \"    instead.\")"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(list(c(\"  \\036 The ‘internal’ graphics device invoked by .Call(\\\"R_GD_nullDevice\\\",\", \"    package = \\\"grDevices\\\") has been removed: use pdf(file = NULL)\", \"    instead.\")), TRUE, TRUE)"));                 
.Internal(unlist(argv[[1]], argv[[2]], argv[[3]]));                 
}, o=expected);                 

