expected <- eval(parse(text="structure(list(mtime = structure(1386583148.91412, class = c(\"POSIXct\", \"POSIXt\"))), .Names = \"mtime\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(size = 131, isdir = FALSE, mode = structure(436L, class = \"octmode\"), mtime = structure(1386583148.91412, class = c(\"POSIXct\", \"POSIXt\")), ctime = structure(1386583148.91712, class = c(\"POSIXct\", \"POSIXt\")), atime = structure(1386583149.16512, class = c(\"POSIXct\", \"POSIXt\")), uid = 501L, gid = 501L, uname = \"lzhao\", grname = \"lzhao\"), .Names = c(\"size\", \"isdir\", \"mode\", \"mtime\", \"ctime\", \"atime\", \"uid\", \"gid\", \"uname\", \"grname\"), class = \"data.frame\", row.names = \"startup.Rs\"), \"mtime\")"));                
do.call(`.subset`, argv);                
}, o=expected);                

