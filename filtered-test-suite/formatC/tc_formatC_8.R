expected <- eval(parse(text="\"48.43\""));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(48.4333681840033, .Names = \"value\"), \"double\", 5L, 4L, \"g\", \"\", 12L)"));   
.Internal(`formatC`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));   
}, o=expected);   

