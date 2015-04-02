expected <- eval(parse(text="structure(list(size = NA_real_, isdir = NA, mode = structure(NA_integer_, class = \"octmode\"), mtime = NA_real_, ctime = NA_real_, atime = NA_real_, uid = NA_integer_, gid = NA_integer_, uname = NA_character_, grname = NA_character_), .Names = c(\"size\", \"isdir\", \"mode\", \"mtime\", \"ctime\", \"atime\", \"uid\", \"gid\", \"uname\", \"grname\"))"));            
test(id=0, code={            
argv <- eval(parse(text="list(\"/home/lzhao/hg/r-instrumented/library/codetools/data\")"));            
.Internal(file.info(argv[[1]]));            
}, o=expected);            

