expected <- eval(parse(text="NA_character_"));       
test(id=0, code={       
argv <- eval(parse(text="list(NA_character_, \"any\")"));       
.Internal(`as.vector`(argv[[1]], argv[[2]]));       
}, o=expected);       

