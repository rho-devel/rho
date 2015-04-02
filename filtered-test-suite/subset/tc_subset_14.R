expected <- eval(parse(text="structure(list(mtime = structure(1393948130.23894, class = c(\"POSIXct\", \"POSIXt\"))), .Names = \"mtime\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(size = 1056, isdir = FALSE, mode = structure(420L, class = \"octmode\"), mtime = structure(1393948130.23894, class = c(\"POSIXct\", \"POSIXt\")), ctime = structure(1393948130.23894, class = c(\"POSIXct\", \"POSIXt\")), atime = structure(1395074550.46596, class = c(\"POSIXct\", \"POSIXt\")), uid = 1001L, gid = 1001L, uname = \"roman\", grname = \"roman\"), .Names = c(\"size\", \"isdir\", \"mode\", \"mtime\", \"ctime\", \"atime\", \"uid\", \"gid\", \"uname\", \"grname\"), class = \"data.frame\", row.names = \"/home/roman/r-instrumented/library/grid/R/grid\"), \"mtime\")"));   
do.call(`.subset`, argv);   
}, o=expected);   

