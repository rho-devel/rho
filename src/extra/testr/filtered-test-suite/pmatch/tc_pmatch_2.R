expected <- eval(parse(text="6L"));    
test(id=0, code={    
argv <- eval(parse(text="list(\"month\", c(\"secs\", \"mins\", \"hours\", \"days\", \"weeks\", \"months\", \"years\", \"DSTdays\"), NA_integer_, FALSE)"));    
.Internal(`pmatch`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));    
}, o=expected);    

