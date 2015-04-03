expected <- eval(parse(text="c(\"aa\", \"row.names\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"^..dfd.\", \"\", c(\"aa\", \"..dfd.row.names\"), FALSE, FALSE, FALSE, FALSE)"));   
.Internal(`sub`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));   
}, o=expected);   

