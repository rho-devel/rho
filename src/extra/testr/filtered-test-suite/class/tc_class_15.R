expected <- eval(parse(text="c(\"summaryDefault\", \"table\")"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(c(-0.00225540511921, -0.00045867962383, -8.86739505379e-06, -1.96554854754e-06, 0.000402346479421, 0.00193962597167), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\"), class = c(\"summaryDefault\", \"table\")))"));              
do.call(`class`, argv);              
}, o=expected);              

