expected <- eval(parse(text="c(Inf, -Inf)"));           
test(id=0, code={           
argv <- eval(parse(text="list(structure(logical(0), .Dim = c(0L, 0L), .Dimnames = list(NULL, NULL)), na.rm = FALSE)"));           
do.call(`range`, argv);           
}, o=expected);           

