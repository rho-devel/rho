expected <- eval(parse(text="NULL"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"header and 'col.names' are of different lengths\", quote(read.table(\"foo3\", header = TRUE, col.names = letters[1:4])))"));   
.Internal(`.dfltWarn`(argv[[1]], argv[[2]]));   
}, o=expected);   

