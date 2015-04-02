expected <- eval(parse(text="integer(0)"));                
test(id=0, code={                
argv <- eval(parse(text="list(character(0), c(\"methods\", \"utils\", \"XML\", \"RCurl\"), 0L, NULL)"));                
.Internal(match(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));                
}, o=expected);                

