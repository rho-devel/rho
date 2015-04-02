expected <- eval(parse(text="c(1L, 1L, 1L, 1L)"));    
test(id=0, code={    
argv <- eval(parse(text="list(c(NA_character_, NA_character_, NA_character_, NA_character_), \"NA\", NA_integer_, TRUE)"));    
.Internal(`pmatch`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));    
}, o=expected);    

