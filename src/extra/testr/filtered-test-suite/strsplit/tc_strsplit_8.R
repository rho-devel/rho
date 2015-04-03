expected <- eval(parse(text="list(\"The \\\\usage entries for S3 methods should use the \\\\method markup and not their full name.\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(\"The \\\\usage entries for S3 methods should use the \\\\method markup and not their full name.\\n\", \"\\n\", FALSE, FALSE, TRUE)"));              
.Internal(strsplit(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));              
}, o=expected);              

