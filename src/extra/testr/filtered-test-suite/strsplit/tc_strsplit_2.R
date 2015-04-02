expected <- eval(parse(text="list(c(\"x\", \",\", \" \", \"r\", \"o\", \"w\", \".\", \"n\", \"a\", \"m\", \"e\", \"s\", \" \", \"=\", \" \", \"N\", \"U\", \"L\", \"L\", \",\", \" \"))"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(\"x, row.names = NULL, \", Rd_tag = \"RCODE\"), \"\", FALSE, FALSE, FALSE)"));              
.Internal(strsplit(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));              
}, o=expected);              

