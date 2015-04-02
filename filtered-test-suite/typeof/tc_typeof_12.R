expected <- eval(parse(text="\"character\""));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(\"Min.   : 1.000  \", \"1st Qu.: 9.000  \", \"Median :18.000  \", \"Mean   :14.742  \", \"3rd Qu.:20.000  \", \"Max.   :23.000  \", NA, \"Min.   :5.0000  \", \"1st Qu.:5.3000  \", \"Median :6.1000  \", \"Mean   :6.0841  \", \"3rd Qu.:6.6000  \", \"Max.   :7.7000  \", NA, \"Min.   :  1.000  \", \"1st Qu.: 24.250  \", \"Median : 56.500  \", \"Mean   : 56.928  \", \"3rd Qu.: 86.750  \", \"Max.   :117.000  \", \"NA's   :16  \", \"Min.   :  0.500  \", \"1st Qu.: 11.325  \", \"Median : 23.400  \", \"Mean   : 45.603  \", \"3rd Qu.: 47.550  \", \"Max.   :370.000  \", NA, \"Min.   :0.00300  \", \"1st Qu.:0.04425  \", \"Median :0.11300  \", \"Mean   :0.15422  \", \"3rd Qu.:0.21925  \", \"Max.   :0.81000  \", NA), .Dim = c(7L, 5L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\", \"\"), c(\"    event\", \"     mag\", \"   station\", \"     dist\", \"    accel\"))))"));         
.Internal(`typeof`(argv[[1]]));         
}, o=expected);         

