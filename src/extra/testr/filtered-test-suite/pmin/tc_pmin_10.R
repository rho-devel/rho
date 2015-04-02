expected <- eval(parse(text="0.341867139159"));           
test(id=0, code={           
argv <- eval(parse(text="list(FALSE, 1, 0.341867139159)"));           
.Internal(pmin(argv[[1]], argv[[2]], argv[[3]]));           
}, o=expected);           

