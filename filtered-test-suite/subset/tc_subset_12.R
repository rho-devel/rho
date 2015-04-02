expected <- eval(parse(text="structure(list(frow = c(32L, 33L, 33L, 36L)), .Names = \"frow\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(srcfile = c(\"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/cloud.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/cloud.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/cloud.R\", \"/home/lzhao/tmp/RtmpGUHe0I/R.INSTALL2aa51f3e9d31/lattice/R/cloud.R\"), frow = c(32L, 33L, 33L, 36L), lrow = c(32L, 33L, 33L, 36L)), .Names = c(\"srcfile\", \"frow\", \"lrow\"), row.names = c(NA, 4L), class = \"data.frame\"), \"frow\")"));                
do.call(`.subset`, argv);                
}, o=expected);                

