expected <- eval(parse(text="\"«Latin-1 accented chars»: ghè øØ å<Å æ<Æ gh ghè\""));                   
test(id=0, code={                   
argv <- eval(parse(text="list(\"é\", \"gh\", \"«Latin-1 accented chars»: éè øØ å<Å æ<Æ é éè\", FALSE, FALSE, FALSE, FALSE)"));                   
.Internal(gsub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));                   
}, o=expected);                   

