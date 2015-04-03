expected <- eval(parse(text="structure(c(-1, 1, -1), .Dim = 3L, .Dimnames = list(c(\"73\", \"312\", \"674\")))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(-Inf, Inf, -Inf), .Dim = 3L, .Dimnames = list(c(\"73\", \"312\", \"674\"))))"));         
do.call(`sign`, argv);         
}, o=expected);         

