expected <- eval(parse(text="2L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(list(structure(list(srcfile = c(\"/home/lzhao/tmp/RtmpYl9n1I/R.INSTALL2aa24b6697e5/MASS/R/rlm.R\", \"/home/lzhao/tmp/RtmpYl9n1I/R.INSTALL2aa24b6697e5/MASS/R/rlm.R\"), frow = 122:123, lrow = 122:123), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2, class = \"data.frame\"), structure(list(srcfile = \"/home/lzhao/tmp/RtmpYl9n1I/R.INSTALL2aa24b6697e5/MASS/R/rlm.R\", frow = 124L, lrow = 124L), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, -1L), class = \"data.frame\")))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

