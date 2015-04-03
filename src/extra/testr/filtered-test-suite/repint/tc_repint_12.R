expected <- eval(parse(text="c(NA_character_, NA_character_, NA_character_, NA_character_, NA_character_)"));     
test(id=0, code={     
argv <- eval(parse(text="list(NA_character_, 5L)"));     
.Internal(`rep.int`(argv[[1]], argv[[2]]));     
}, o=expected);     

