expected <- eval(parse(text="c(\"***\", \"*  \", \"   \", \"   \", \"   \")"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(c(\"***\", \"*\", \" \", \" \", \" \"), legend = \"0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\", class = \"noquote\"), FALSE, NULL, 0L, NULL, 0L, TRUE, NA)"));            
.Internal(format(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));            
}, o=expected);            

