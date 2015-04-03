expected <- eval(parse(text="FALSE"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(\"Min.   :10.00  \", \"1st Qu.:15.25  \", \"Median :20.50  \", \"Mean   :21.67  \", \"3rd Qu.:25.50  \", \"Max.   :43.00  \", \"A:9  \", \"B:9  \", NA, NA, NA, NA), .Dim = c(6L, 2L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\"), c(\"    breaks\", \"wool\"))))"));              
do.call(`is.expression`, argv);              
}, o=expected);              

