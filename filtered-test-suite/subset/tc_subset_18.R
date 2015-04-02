expected <- eval(parse(text="structure(list(lrow = c(NA, NA, 428L, 428L, 432L, 437L, 437L, 441L, 441L)), .Names = \"lrow\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(srcfile = c(NA, NA, \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/levelplot.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/levelplot.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/levelplot.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/levelplot.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/levelplot.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/levelplot.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/levelplot.R\"), frow = c(NA, NA, 427L, 427L, 432L, 434L, 434L, 438L, 438L), lrow = c(NA, NA, 428L, 428L, 432L, 437L, 437L, 441L, 441L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, 9L), class = \"data.frame\"), \"lrow\")"));                
do.call(`.subset`, argv);                
}, o=expected);                

