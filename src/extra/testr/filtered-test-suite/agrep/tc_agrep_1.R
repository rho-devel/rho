expected <- eval(parse(text="1L"));  
test(id=0, code={  
argv <- eval(parse(text="list(\"x86_64-linux-gnu\", \"x86_64-linux-gnu\", FALSE, FALSE, c(1L, 1L, 1L), c(0.1, NA, NA, NA, NA), FALSE, TRUE)"));  
.Internal(`agrep`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));  
}, o=expected);  

