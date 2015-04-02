expected <- eval(parse(text="structure(1L, match.length = 14L, useBytes = TRUE)"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"^.*\\\\{n\", \"my(ugly[file{name\", FALSE, FALSE, FALSE, FALSE)"));  
.Internal(`regexpr`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));  
}, o=expected);  

