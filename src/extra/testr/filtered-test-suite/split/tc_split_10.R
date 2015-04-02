expected <- eval(parse(text="structure(list(), .Names = character(0))"));  
test(id=0, code={  
argv <- eval(parse(text="list(character(0), structure(integer(0), .Label = character(0), class = \"factor\"))"));  
.Internal(`split`(argv[[1]], argv[[2]]));  
}, o=expected);  

