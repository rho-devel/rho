expected <- eval(parse(text="c(0.944550219923258, 0.336629745550454, 0.629688071087003, 0.591416056267917)"));     
test(id=0, code={     
argv <- eval(parse(text="list(c(0.944550219923258, 0.336629745550454, 0.629688071087003, 0.591416056267917), NULL, TRUE, NULL, NULL, FALSE, NULL, TRUE, TRUE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

