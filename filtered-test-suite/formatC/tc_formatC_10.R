expected <- eval(parse(text="c(\"20\", \"30\", \"40\", \"50\", \"60\", \"70\", \"80\", \"90\", \"100\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(c(20, 30, 40, 50, 60, 70, 80, 90, 100), \"double\", 1, 7L, \"fg\", \"\", c(15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L, 15L))"));   
.Internal(`formatC`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));   
}, o=expected);   

