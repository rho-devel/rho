expected <- eval(parse(text="structure(list(str = structure(list(strict.width = \"no\", digits.d = 3, vec.len = 4), .Names = c(\"strict.width\", \"digits.d\", \"vec.len\"))), .Names = \"str\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(\"str\")"));   
.Internal(`options`(argv[[1]]));   
}, o=expected);   

