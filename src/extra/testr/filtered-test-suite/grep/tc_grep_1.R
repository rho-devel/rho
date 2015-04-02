expected <- eval(parse(text="integer(0)"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"|\", \"wool\", FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)"));     
.Internal(`grep`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));     
}, o=expected);     

