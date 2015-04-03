expected <- eval(parse(text="c(\"@CRAN@\", \"http://www.stats.ox.ac.uk/pub/RWin\", \"http://www.bioconductor.org/packages/%v/bioc\", \"http://www.bioconductor.org/packages/%v/data/annotation\", \"http://www.bioconductor.org/packages/%v/data/experiment\", \"http://www.bioconductor.org/packages/%v/extra\", \"http://www.omegahat.org/R\", \"http://R-Forge.R-project.org\", \"http://www.rforge.net\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(\"%bm\", \"http://www.bioconductor.org\", c(\"@CRAN@\", \"http://www.stats.ox.ac.uk/pub/RWin\", \"%bm/packages/%v/bioc\", \"%bm/packages/%v/data/annotation\", \"%bm/packages/%v/data/experiment\", \"%bm/packages/%v/extra\", \"http://www.omegahat.org/R\", \"http://R-Forge.R-project.org\", \"http://www.rforge.net\"), FALSE, FALSE, TRUE, FALSE)"));               
.Internal(sub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));               
}, o=expected);               

