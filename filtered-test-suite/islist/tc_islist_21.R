expected <- eval(parse(text="TRUE"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/tmp/Rtmpe5iuYI/R.INSTALL2aa854a74188/foreign/R/R_systat.R\", \"/home/lzhao/tmp/Rtmpe5iuYI/R.INSTALL2aa854a74188/foreign/R/R_systat.R\"), frow = 21:22, lrow = 21:22), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2))"));                  
do.call(`is.list`, argv);                  
}, o=expected);                  

