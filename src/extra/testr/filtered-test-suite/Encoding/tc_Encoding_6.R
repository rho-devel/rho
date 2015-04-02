expected <- eval(parse(text="\"unknown\""));  
test(id=0, code={  
argv <- eval(parse(text="list(\"A shell of class documentation has been written to the file './myTst2/man/DocLink-class.Rd'.\\n\")"));  
.Internal(`Encoding`(argv[[1]]));  
}, o=expected);  

