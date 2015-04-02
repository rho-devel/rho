expected <- eval(parse(text="187L"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(srcfile = \"/home/lzhao/tmp/RtmpS45wYI/R.INSTALL2aa62411bcd3/rpart/R/rpart.R\", frow = 187L, lrow = 187L), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, -1L), class = \"data.frame\"), 2L)"));                 
do.call(`.subset2`, argv);                 
}, o=expected);                 

