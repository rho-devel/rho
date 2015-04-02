expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(\"^[[:space:]]*## No test:\", \"Diagonal(3)\", FALSE, FALSE, TRUE, FALSE, TRUE, FALSE)"));                 
.Internal(grepl(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));                 
}, o=expected);                 

