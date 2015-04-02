expected <- eval(parse(text="\"^*\\\\.t??$\""));                   
test(id=0, code={                   
argv <- eval(parse(text="list(\"\\\\.\", \"\\\\\\\\.\", \"^*.t??$\", FALSE, FALSE, FALSE, FALSE)"));                   
.Internal(gsub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));                   
}, o=expected);                   

