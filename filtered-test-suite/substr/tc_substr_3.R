expected <- eval(parse(text="structure(c(\"as.for\", \"coef\", \"makepr\", \"na.fai\", \"predic\"), .Names = c(\"as.formula\", \"coef\", \"makepredictcall\", \"na.fail\", \"predict\"))"));    
test(id=0, code={    
argv <- eval(parse(text="list(structure(c(\"as.formula\", \"coef\", \"makepredictcall\", \"na.fail\", \"predict\"), .Names = c(\"as.formula\", \"coef\", \"makepredictcall\", \"na.fail\", \"predict\")), 1L, 6L)"));    
.Internal(`substr`(argv[[1]], argv[[2]], argv[[3]]));    
}, o=expected);    

