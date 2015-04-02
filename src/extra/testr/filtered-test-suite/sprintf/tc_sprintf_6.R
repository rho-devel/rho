expected <- eval(parse(text="\"p,L,S = ( 1, 1, 0): \""));           
test(id=0, code={           
argv <- eval(parse(text="list(\"p,L,S = (%2d,%2d,%2d): \", TRUE, TRUE, FALSE)"));           
.Internal(sprintf(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));           
}, o=expected);           

