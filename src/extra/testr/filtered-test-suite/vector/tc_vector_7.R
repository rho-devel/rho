expected <- eval(parse(text="list(NULL)"));           
test(id=0, code={           
argv <- eval(parse(text="list(\"list\", structure(1L, .Names = \"\\\\c\"))"));           
.Internal(vector(argv[[1]], argv[[2]]));           
}, o=expected);           

