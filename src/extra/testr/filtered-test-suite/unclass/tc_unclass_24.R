expected <- eval(parse(text="structure(c(\"Min.   :14.00  \", \"1st Qu.:26.00  \", \"Median :29.50  \", \"Mean   :36.39  \", \"3rd Qu.:49.25  \", \"Max.   :70.00  \", \"A:9  \", \"B:9  \", NA, NA, NA, NA), .Dim = c(6L, 2L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\"), c(\"    breaks\", \"wool\")))"));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(c(\"Min.   :14.00  \", \"1st Qu.:26.00  \", \"Median :29.50  \", \"Mean   :36.39  \", \"3rd Qu.:49.25  \", \"Max.   :70.00  \", \"A:9  \", \"B:9  \", NA, NA, NA, NA), .Dim = c(6L, 2L), .Dimnames = list(c(\"\", \"\", \"\", \"\", \"\", \"\"), c(\"    breaks\", \"wool\"))))"));                 
do.call(`unclass`, argv);                 
}, o=expected);                 

