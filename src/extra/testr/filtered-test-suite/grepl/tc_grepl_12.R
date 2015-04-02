expected <- eval(parse(text="logical(0)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(\"^prepare_Rd\", structure(character(0), class = \"checkRd\"), FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));                 
.Internal(grepl(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));                 
}, o=expected);                 

