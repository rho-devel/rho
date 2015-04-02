expected <- eval(parse(text="2:4"));  
test(id=0, code={  
argv <- eval(parse(text="list(c(\"x\", \"y\", \"z\"), c(\"row.names\", \"x\", \"y\", \"z\"), 0L)"));  
.Internal(`charmatch`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

