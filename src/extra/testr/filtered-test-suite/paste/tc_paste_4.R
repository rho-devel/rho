expected <- eval(parse(text="\"package:pkgA\""));       
test(id=0, code={       
argv <- eval(parse(text="list(list(\"package\", structure(\"pkgA\", .Names = \"name\")), \":\", NULL)"));       
.Internal(`paste`(argv[[1]], argv[[2]], argv[[3]]));       
}, o=expected);       

