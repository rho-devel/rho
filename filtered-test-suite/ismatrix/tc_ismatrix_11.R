expected <- eval(parse(text="FALSE"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/tmp/Rtmpe5iuYI/R.INSTALL2aa854a74188/foreign/R/arff.R\", \"/home/lzhao/tmp/Rtmpe5iuYI/R.INSTALL2aa854a74188/foreign/R/arff.R\"), frow = c(86L, 86L), lrow = c(88L, 88L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2, class = \"data.frame\"))"));                 
do.call(`is.matrix`, argv);                 
}, o=expected);                 

