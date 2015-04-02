expected <- eval(parse(text="\"character\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(\"Min.   :    0.060  \", \"1st Qu.:    0.320  \", \"Median :    0.630  \", \"Mean   :  909.592  \", \"3rd Qu.:    0.905  \", \"Max.   :10000.000  \"), .Dim = c(6L, 1L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\"), \"      x\")))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

