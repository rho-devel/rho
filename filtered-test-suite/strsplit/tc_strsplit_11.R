expected <- eval(parse(text="structure(list(name = \"pkgB\"), .Names = \"name\")"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(\"pkgB\", .Names = \"name\"), \"_\", TRUE, FALSE, FALSE)"));    
.Internal(`strsplit`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
}, o=expected);    

