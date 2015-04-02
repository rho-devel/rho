expected <- eval(parse(text="structure(list(srcfile = c(\"/home/lzhao/hg/r-instrumented/library/base/R/base\", \"/home/lzhao/hg/r-instrumented/library/base/R/base\"), frow = 5852:5853, lrow = c(5852L, 5854L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2)"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/hg/r-instrumented/library/base/R/base\", \"/home/lzhao/hg/r-instrumented/library/base/R/base\"), frow = 5852:5853, lrow = c(5852L, 5854L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

