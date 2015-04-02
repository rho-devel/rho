expected <- eval(parse(text="structure(list(srcfile = \"/home/lzhao/hg/r-instrumented/library/utils/R/utils\", frow = 1271L, lrow = 1273L), .Names = c(\"srcfile\", \"frow\", \"lrow\"))"));                  
test(id=0, code={                  
argv <- eval(parse(text="list(srcfile = \"/home/lzhao/hg/r-instrumented/library/utils/R/utils\", frow = 1271L, lrow = 1273L)"));                  
do.call(`list`, argv);                  
}, o=expected);                  

