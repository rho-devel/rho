expected <- eval(parse(text="structure(\"base\", .Names = \"Priority\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(\"base\", .Names = \"Priority\"))"));       
.Internal(tolower(argv[[1]]));       
}, o=expected);       

