expected <- eval(parse(text="c(\"\", \"\", \"REMISSION\", \"\", \"\", \"\", \"\", \"\", \"\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(c(\"\", \"\", \"remission\", \"\", \"\", \"\", \"\", \"\", \"\"))"));          
.Internal(toupper(argv[[1]]));          
}, o=expected);          

