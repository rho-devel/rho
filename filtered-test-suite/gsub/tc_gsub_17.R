expected <- eval(parse(text="\"A Test Of Capitalizing\""));                   
test(id=0, code={                   
argv <- eval(parse(text="list(\"(\\\\w)(\\\\w*)\", \"\\\\U\\\\1\\\\L\\\\2\", \"a test of capitalizing\", FALSE, TRUE, FALSE, FALSE)"));                   
.Internal(gsub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));                   
}, o=expected);                   

