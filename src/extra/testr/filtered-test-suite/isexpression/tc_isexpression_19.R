expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(-Inf, -Inf, -2.248e+263, -Inf, -3.777e+116, -1), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\")))"));       
do.call(`is.expression`, argv);       
}, o=expected);       

