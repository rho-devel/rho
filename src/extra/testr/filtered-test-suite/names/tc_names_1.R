expected <- eval(parse(text="c(\"size\", \"isdir\", \"mode\", \"mtime\", \"ctime\", \"atime\", \"uid\", \"gid\", \"uname\", \"grname\")"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(list(size = 113, isdir = FALSE, mode = structure(436L, class = \"octmode\"), mtime = structure(1395082088.72988, class = c(\"POSIXct\", \"POSIXt\")), ctime = structure(1395082088.72988, class = c(\"POSIXct\", \"POSIXt\")), atime = structure(1395082088.77388, class = c(\"POSIXct\", \"POSIXt\")), uid = 1001L, gid = 1001L, uname = \"roman\", grname = \"roman\"), .Names = c(\"size\", \"isdir\", \"mode\", \"mtime\", \"ctime\", \"atime\", \"uid\", \"gid\", \"uname\", \"grname\"), class = \"data.frame\", row.names = \"/tmp/RtmptPgrXI/file55711ba85492\"))"));         
do.call(`names`, argv);         
}, o=expected);         

