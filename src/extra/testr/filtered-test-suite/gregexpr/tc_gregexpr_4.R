expected <- eval(parse(text="list(structure(c(29L, 54L), match.length = c(4L, 4L), useBytes = TRUE))"));      
test(id=0, code={      
argv <- eval(parse(text="list(\"éè\", \"«Latin-1 accented chars»: éè øØ å<Å æ<Æ é éè\", FALSE, FALSE, TRUE, TRUE)"));      
.Internal(gregexpr(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]]));      
}, o=expected);      

