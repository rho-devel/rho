expected <- eval(parse(text="structure(1:3, class = \"myClass\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(1:3, class = \"myClass\"), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

