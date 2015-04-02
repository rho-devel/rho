expected <- eval(parse(text="c(\"/home/lzhao/hg/r-instrumented/src/library/parallel/R/unix/forkCluster.R\", \"/home/lzhao/hg/r-instrumented/src/library/parallel/R/unix/mcfork.R\", \"/home/lzhao/hg/r-instrumented/src/library/parallel/R/unix/mclapply.R\", \"/home/lzhao/hg/r-instrumented/src/library/parallel/R/unix/mcmapply.R\", \"/home/lzhao/hg/r-instrumented/src/library/parallel/R/unix/mcparallel.R\", \"/home/lzhao/hg/r-instrumented/src/library/parallel/R/unix/pvec.R\")"));            
test(id=0, code={            
argv <- eval(parse(text="list(list(\"/home/lzhao/hg/r-instrumented/src/library/parallel/R/unix\", c(\"forkCluster.R\", \"mcfork.R\", \"mclapply.R\", \"mcmapply.R\", \"mcparallel.R\", \"pvec.R\")), \"/\")"));            
.Internal(file.path(argv[[1]], argv[[2]]));            
}, o=expected);            

