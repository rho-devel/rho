expected <- eval(parse(text="raw(0)"));     
test(id=0, code={     
argv <- eval(parse(text="list(\"\")"));     
.Internal(charToRaw(argv[[1]]));     
}, o=expected);     

