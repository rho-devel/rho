expected <- eval(parse(text="structure(list(size = numeric(0), isdir = logical(0), mode = structure(integer(0), class = \"octmode\"), mtime = numeric(0), ctime = numeric(0), atime = numeric(0), uid = integer(0), gid = integer(0), uname = character(0), grname = character(0)), .Names = c(\"size\", \"isdir\", \"mode\", \"mtime\", \"ctime\", \"atime\", \"uid\", \"gid\", \"uname\", \"grname\"))"));            
test(id=0, code={            
argv <- eval(parse(text="list(character(0))"));            
.Internal(file.info(argv[[1]]));            
}, o=expected);            

