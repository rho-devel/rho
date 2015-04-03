expected <- eval(parse(text="structure(c(\"\", \" 1\", \" 1\", \" 1\", \"\", \"  9.93\", \" 26.79\", \"820.91\", \" 47.97\", \" 57.90\", \" 74.76\", \"868.88\", \"24.974\", \"25.420\", \"28.742\", \"60.629\"), .Dim = c(4L, 4L), .Dimnames = list(c(\"<none>\", \"- x4\", \"- x2\", \"- x1\"), c(\"Df\", \"Sum of Sq\", \"RSS\", \"AIC\")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"\", \" 1\", \" 1\", \" 1\", \"\", \"  9.93\", \" 26.79\", \"820.91\", \" 47.97\", \" 57.90\", \" 74.76\", \"868.88\", \"24.974\", \"25.420\", \"28.742\", \"60.629\"), .Dim = c(4L, 4L), .Dimnames = list(c(\"<none>\", \"- x4\", \"- x2\", \"- x1\"), c(\"Df\", \"Sum of Sq\", \"RSS\", \"AIC\"))), NULL, FALSE, \"\", NULL, TRUE, NULL, TRUE, FALSE)"));     
.Internal(`print.default`(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]], argv[[6]], argv[[7]], argv[[8]], argv[[9]]));     
}, o=expected);     

