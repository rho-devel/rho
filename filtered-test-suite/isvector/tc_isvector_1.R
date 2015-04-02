expected <- eval(parse(text="TRUE"));        
test(id=0, code={        
argv <- eval(parse(text="list(list(structure(0, class = c(\"POSIXct\", \"POSIXt\"), tzone = \"GMT\"), 1262304000), \"any\")"));        
.Internal(`is.vector`(argv[[1]], argv[[2]]));        
}, o=expected);        

