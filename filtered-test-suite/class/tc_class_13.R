expected <- eval(parse(text="c(\"summaryDefault\", \"table\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(3.1, 6.695, 8.14, 7.50090909091, 8.95, 9.26), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\"), class = c(\"summaryDefault\", \"table\")))"));              
do.call(`class`, argv);              
}, o=expected);              

