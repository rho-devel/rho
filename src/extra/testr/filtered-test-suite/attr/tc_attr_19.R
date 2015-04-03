expected <- eval(parse(text="1:6"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\", \"/home/lzhao/hg/r-instrumented/library/stats/R/stats\"), frow = c(2418L, 2418L, 2418L, 2421L, 2422L, 2424L), lrow = c(2418L, 2418L, 2418L, 2421L, 2426L, 2424L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, 6L), class = \"data.frame\"), \"row.names\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

