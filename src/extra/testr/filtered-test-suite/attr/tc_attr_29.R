expected <- eval(parse(text="1:2"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/hg/r-instrumented/library/graphics/R/graphics\", \"/home/lzhao/hg/r-instrumented/library/graphics/R/graphics\"), frow = 4262:4263, lrow = 4262:4263), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = 1:2, class = \"data.frame\"), \"row.names\")"));                   
do.call(`attr`, argv);                   
}, o=expected);                   

