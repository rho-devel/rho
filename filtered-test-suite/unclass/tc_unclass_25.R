expected <- eval(parse(text="structure(list(srcfile = \"/home/lzhao/tmp/RtmpYl9n1I/R.INSTALL2aa24b6697e5/MASS/R/negbin.R\", frow = 135L, lrow = 137L), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, -1L))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(srcfile = \"/home/lzhao/tmp/RtmpYl9n1I/R.INSTALL2aa24b6697e5/MASS/R/negbin.R\", frow = 135L, lrow = 137L), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, -1L)))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

