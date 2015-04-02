expected <- eval(parse(text="c(\"Alb\", \"Als\", \"Arz\", \"Ark\", \"Clf\", \"Clr\", \"Cn\", \"Dl\", \"Fl\", \"Gr\", \"Hw\", \"Id\", \"Il\", \"In\", \"Iw\", \"Kns\", \"Knt\", \"Ls\", \"Man\", \"Mr\", \"Mssc\", \"Mc\", \"Mnn\", \"Msss\", \"Mssr\", \"Mnt\", \"Nb\", \"Nv\", \"NH\", \"NJ\", \"NM\", \"NY\", \"NC\", \"ND\", \"Oh\", \"Ok\", \"Or\", \"Pn\", \"RI\", \"SC\", \"SD\", \"Tn\", \"Tx\", \"Ut\", \"Vrm\", \"Vrg\", \"Wsh\", \"WV\", \"Wsc\", \"Wy\")"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(\"Alb\", \"Als\", \"Arz\", \"Ark\", \"Clf\", \"Clr\", \"Cn\", \"Dl\", \"Fl\", \"Gr\", \"Hw\", \"Id\", \"Il\", \"In\", \"Iw\", \"Kns\", \"Knt\", \"Ls\", \"Man\", \"Mr\", \"Mssc\", \"Mc\", \"Mnn\", \"Msss\", \"Mssr\", \"Mnt\", \"Nb\", \"Nv\", \"NH\", \"NJ\", \"NM\", \"NY\", \"NC\", \"ND\", \"Oh\", \"Ok\", \"Or\", \"Pn\", \"RI\", \"SC\", \"SD\", \"Tn\", \"Tx\", \"Ut\", \"Vrm\", \"Vrg\", \"Wsh\", \"WV\", \"Wsc\", \"Wy\"), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

