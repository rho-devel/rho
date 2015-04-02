expected <- eval(parse(text="1L"));                
test(id=0, code={                
argv <- eval(parse(text="list(\"[\", \"^\\\\.__[MT]\", FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)"));                
.Internal(grep(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]]));                
}, o=expected);                

