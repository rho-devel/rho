expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(1, 24.25, 56.5, 56.92771, 86.75, 117), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\")))"));       
do.call(`is.expression`, argv);       
}, o=expected);       

