expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(k = 0.413311097189709, g1 = 72.8306629700373, g2 = 181.793153976139), .Names = c(\"k\", \"g1\", \"g2\")), TRUE)"));       
.Internal(`islistfactor`(argv[[1]], argv[[2]]));       
}, o=expected);       

