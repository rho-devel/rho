expected <- eval(parse(text="integer(0)"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"^[[:blank:]]*$\", \"mtext(\\\"«Latin-1 accented chars»: éè øØ å<Å æ<Æ\\\", side = 3)\", FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)"));     
.Internal(`grep`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));     
}, o=expected);     

