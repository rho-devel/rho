expected <- eval(parse(text="2L"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"lasy\", c(\" 1 lazy 2\", \"1 lasy 2\"), FALSE, FALSE, c(1L, 1L, 1L), structure(c(NA, 0.1, 0.1, 0, 0.1), .Names = c(\"cost\", \"insertions\", \"deletions\", \"substitutions\", \"all\")), FALSE, TRUE)"));    
.Internal(agrep(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));    
}, o=expected);    

