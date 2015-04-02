expected <- eval(parse(text="structure(c(\"Min.   : 1.00  \", \"1st Qu.: 3.25  \", \"Median : 5.50  \", \"Mean   : 5.50  \", \"3rd Qu.: 7.75  \", \"Max.   :10.00  \", \"Min.   : 1.00    Min.   :11.00  \", \"1st Qu.: 3.25    1st Qu.:13.25  \", \"Median : 5.50    Median :15.50  \", \"Mean   : 5.50    Mean   :15.50  \", \"3rd Qu.: 7.75    3rd Qu.:17.75  \", \"Max.   :10.00    Max.   :20.00  \"), .Dim = c(6L, 2L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\"), c(\"    X1.10\", \"      z.x             z.yyy     \")))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(\"Min.   : 1.00  \", \"1st Qu.: 3.25  \", \"Median : 5.50  \", \"Mean   : 5.50  \", \"3rd Qu.: 7.75  \", \"Max.   :10.00  \", \"Min.   : 1.00    Min.   :11.00  \", \"1st Qu.: 3.25    1st Qu.:13.25  \", \"Median : 5.50    Median :15.50  \", \"Mean   : 5.50    Mean   :15.50  \", \"3rd Qu.: 7.75    3rd Qu.:17.75  \", \"Max.   :10.00    Max.   :20.00  \"), .Dim = c(6L, 2L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\"), c(\"    X1.10\", \"      z.x             z.yyy     \"))))"));     
do.call(`unclass`, argv);     
}, o=expected);     

