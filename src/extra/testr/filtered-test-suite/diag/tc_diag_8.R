expected <- eval(parse(text="structure(c(0.553622032575332, 0, 0, 0, 0, 1.83583330034692, 0, 0, 0, 0, 0.540309168173204, 0, 0, 0, 0, 0.347171956892285), .Dim = c(4L, 4L))"));  
test(id=0, code={  
argv <- eval(parse(text="list(structure(c(0.553622032575332, 1.83583330034692, 0.540309168173204, 0.347171956892285), .Names = c(\"A\", \"B\", \"C\", \"D\")), 4L, 4L)"));  
.Internal(`diag`(argv[[1]], argv[[2]], argv[[3]]));  
}, o=expected);  

