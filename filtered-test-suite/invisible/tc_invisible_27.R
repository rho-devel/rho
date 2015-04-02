expected <- eval(parse(text="structure(c(\"Min.   :  5.00  \", \"1st Qu.: 12.50  \", \"Median : 23.00  \", \"Mean   : 29.48  \", \"3rd Qu.: 33.50  \", \"Max.   :161.00  \", \"Min.   :0.0000  \", \"1st Qu.:1.0000  \", \"Median :1.0000  \", \"Mean   :0.7826  \", \"3rd Qu.:1.0000  \", \"Max.   :1.0000  \", \"Maintained   :11  \", \"Nonmaintained:12  \", NA, NA, NA, NA), .Dim = c(6L, 3L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\"), c(\"     time\", \"    status\", \"            x\")), class = \"table\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(\"Min.   :  5.00  \", \"1st Qu.: 12.50  \", \"Median : 23.00  \", \"Mean   : 29.48  \", \"3rd Qu.: 33.50  \", \"Max.   :161.00  \", \"Min.   :0.0000  \", \"1st Qu.:1.0000  \", \"Median :1.0000  \", \"Mean   :0.7826  \", \"3rd Qu.:1.0000  \", \"Max.   :1.0000  \", \"Maintained   :11  \", \"Nonmaintained:12  \", NA, NA, NA, NA), .Dim = c(6L, 3L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\"), c(\"     time\", \"    status\", \"            x\")), class = \"table\"))"));             
do.call(`invisible`, argv);             
}, o=expected);             

