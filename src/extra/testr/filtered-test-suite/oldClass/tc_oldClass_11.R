expected <- eval(parse(text="\"data.frame\""));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/tmp/RtmpS45wYI/R.INSTALL2aa62411bcd3/rpart/R/rsq.rpart.R\", \"/home/lzhao/tmp/RtmpS45wYI/R.INSTALL2aa62411bcd3/rpart/R/rsq.rpart.R\"), frow = c(7L, 9L), lrow = c(7L, 9L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2, class = \"data.frame\"))"));                
do.call(`oldClass`, argv);                
}, o=expected);                

