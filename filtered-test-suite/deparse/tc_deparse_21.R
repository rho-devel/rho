expected <- eval(parse(text="\".Primitive(\\\"interactive\\\")\""));          
test(id=0, code={          
argv <- eval(parse(text="list(.Primitive(\"interactive\"), 60L, TRUE, 69, -1L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

