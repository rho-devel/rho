expected <- eval(parse(text="TRUE"));        
test(id=0, code={        
argv <- eval(parse(text="list(list(c(\" 1\", \"NA\", \" 1\"), c(\"1.1\", \" NA\", \"2.0\"), c(\"1.1+0i\", \"    NA\", \"3.0+0i\"), c(\"NA\", \"NA\", \"NA\"), c(\"FALSE\", \"   NA\", \" TRUE\"), c(\"abc\", NA, \"def\")), \"any\")"));        
.Internal(`is.vector`(argv[[1]], argv[[2]]));        
}, o=expected);        

