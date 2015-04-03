expected <- eval(parse(text="\"vi\""));          
test(id=0, code={          
argv <- eval(parse(text="list(\"EDITOR\", \"\")"));          
.Internal(Sys.getenv(argv[[1]], argv[[2]]));          
}, o=expected);          

