expected <- eval(parse(text="structure(list(contrasts = structure(c(\"contr.treatment\", \"contr.poly\"), .Names = c(\"unordered\", \"ordered\"))), .Names = \"contrasts\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"contrasts\")"));   
.Internal(`options`(argv[[1]]));   
}, o=expected);   

