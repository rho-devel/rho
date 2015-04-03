expected <- eval(parse(text="structure(\"     knots).\\n\", Rd_tag = \"TEXT\")"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(\"\\\\bsl\", \"\\\\bsl{}\", structure(\"     knots).\\n\", Rd_tag = \"TEXT\"), FALSE, FALSE, TRUE, TRUE)"));                   
.Internal(gsub(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]]));                   
}, o=expected);                   

