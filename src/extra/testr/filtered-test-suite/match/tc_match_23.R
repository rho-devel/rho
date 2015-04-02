expected <- eval(parse(text="0L"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(\"tools\", .Names = \"name\"), c(\"base\", \"utils\"), 0L, NULL)"));        
.Internal(`match`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

