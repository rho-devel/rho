expected <- eval(parse(text="integer(0)"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(integer(0), .Label = character(0), class = \"factor\"), TRUE, FALSE)"));         
.Internal(radixsort(argv[[1]], argv[[2]], argv[[3]]));         
}, o=expected);         

