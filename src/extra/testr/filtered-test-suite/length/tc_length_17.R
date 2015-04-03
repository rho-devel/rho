expected <- eval(parse(text="3L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/hg/r-instrumented/library/utils/R/utils\", \"/home/lzhao/hg/r-instrumented/library/utils/R/utils\", \"/home/lzhao/hg/r-instrumented/library/utils/R/utils\", \"/home/lzhao/hg/r-instrumented/library/utils/R/utils\", \"/home/lzhao/hg/r-instrumented/library/utils/R/utils\", \"/home/lzhao/hg/r-instrumented/library/utils/R/utils\", \"/home/lzhao/hg/r-instrumented/library/utils/R/utils\"), frow = c(1809L, 1810L, 1811L, 1812L, 1802L, 1827L, 1840L), lrow = c(1809L, 1814L, 1811L, 1813L, 1816L, 1834L, 1842L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, 7L)))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

