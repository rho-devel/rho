expected <- eval(parse(text="c(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"logical\", 15L)"));   
.Internal(`vector`(argv[[1]], argv[[2]]));   
}, o=expected);   

