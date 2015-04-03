expected <- eval(parse(text="list(c(\"A\", \"shell\", \"of\", \"class\", \"documentation\", \"has\", \"been\", \"written\", \"to\", \"the\", \"file\", \"'./myTst2/man/DocLink-class.Rd'.\"))"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"A shell of class documentation has been written to the file './myTst2/man/DocLink-class.Rd'.\\n\", \"[ \\t\\n]\", FALSE, TRUE, TRUE)"));    
.Internal(`strsplit`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));    
}, o=expected);    

