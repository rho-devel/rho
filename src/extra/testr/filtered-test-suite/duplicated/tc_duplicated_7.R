expected <- eval(parse(text="FALSE"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(\"lattice\", .Names = \"\"), FALSE, FALSE, NA)"));  
.Internal(`duplicated`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));  
}, o=expected);  

