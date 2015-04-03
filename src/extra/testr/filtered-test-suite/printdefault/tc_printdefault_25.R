expected <- eval(parse(text="c(\"surname\", \"nationality\", \"deceased\", \"title\", \"other.author\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"surname\", \"nationality\", \"deceased\", \"title\", \"other.author\"), NULL, FALSE, NULL, NULL, FALSE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

