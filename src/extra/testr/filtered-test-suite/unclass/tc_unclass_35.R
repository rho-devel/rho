expected <- eval(parse(text="structure(c(2671, 6.026e+77, 3.161e+152, 3.501e+299, 2.409e+227, 1.529e+302), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\"))"));     
test(id=0, code={     
argv <- eval(parse(text="list(structure(c(2671, 6.026e+77, 3.161e+152, 3.501e+299, 2.409e+227, 1.529e+302), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\")))"));     
do.call(`unclass`, argv);     
}, o=expected);     

