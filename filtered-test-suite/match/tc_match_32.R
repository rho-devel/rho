expected <- eval(parse(text="c(0L, 0L, 0L, 0L, 0L, 0L)"));        
test(id=0, code={        
argv <- eval(parse(text="list(c(\".__C__classA\", \".__T__$:base\", \".__T__$<-:base\", \".__T__[:base\", \".__T__plot:graphics\", \"plot\"), c(\".__NAMESPACE__.\", \".__S3MethodsTable__.\", \".packageName\", \".First.lib\", \".Last.lib\", \".onLoad\", \".onAttach\", \".onDetach\", \".conflicts.OK\", \".noGenerics\"), 0L, NULL)"));        
.Internal(`match`(argv[[1]], argv[[2]], argv[[3]], argv[[4]]));        
}, o=expected);        

