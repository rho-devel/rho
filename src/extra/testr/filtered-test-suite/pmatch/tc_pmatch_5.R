expected <- eval(parse(text="0L"));            
test(id=0, code={            
argv <- eval(parse(text="list(\"fanny.object.\", \"fanny.object\", 0L, FALSE)"));            
.Internal(pmatch(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));            
}, o=expected);            

