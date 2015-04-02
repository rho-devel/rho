expected <- eval(parse(text="c(TRUE, TRUE)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\"BIBINPUTS\", \"add\"), c(\".:.:/home/lzhao/hg/r-instrumented/share/texmf/bibtex/bib::/home/lzhao/hg/r-instrumented/share/texmf/bibtex/bib:\", \"TRUE\"))"));        
.Internal(Sys.setenv(argv[[1]], argv[[2]]));        
}, o=expected);        

