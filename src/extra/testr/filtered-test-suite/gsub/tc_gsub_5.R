expected <- eval(parse(text="structure(\"2013-03-19 13:18:58\", .Names = \"Date/Publication\")"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(\"([{}$#_])\", \"\\\\\\\\\\\\1\", structure(\"2013-03-19 13:18:58\", .Names = \"Date/Publication\"), FALSE, FALSE, FALSE, TRUE)"));                   
.Internal(gsub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));                   
}, o=expected);                   

