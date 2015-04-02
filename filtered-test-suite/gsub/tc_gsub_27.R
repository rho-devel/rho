expected <- eval(parse(text="structure(\"foo\", .Names = \"object\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"%\", \"\\\\\\\\%\", structure(\"foo\", .Names = \"object\"), FALSE, FALSE, FALSE, FALSE)"));     
.Internal(`gsub`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));     
}, o=expected);     

