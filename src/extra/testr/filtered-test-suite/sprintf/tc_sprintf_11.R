expected <- eval(parse(text="c(\"0 0 0\", \"1 1 1\", \"2 2 2\", \"3 3 3\", \"4 4 4\", \"5 5 5\", \"6 6 6\", \"7 7 7\", \"8 8 8\", \"9 9 9\", \"10 a A\", \"11 b B\", \"12 c C\", \"13 d D\", \"14 e E\", \"15 f F\")"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"%1$d %1$x %1$X\", 0:15)"));           
.Internal(sprintf(argv[[1]], argv[[2]]));           
}, o=expected);           

