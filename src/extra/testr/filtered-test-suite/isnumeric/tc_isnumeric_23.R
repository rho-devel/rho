expected <- eval(parse(text="TRUE"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(-3.001e+155, -1.067e+107, -1.976e+62, -9.961e+152, -2.059e+23, 0.5104), .Names = c(\"Min.\", \"1st Qu.\", \"Median\", \"Mean\", \"3rd Qu.\", \"Max.\"), class = c(\"summaryDefault\", \"table\")))"));      
do.call(`is.numeric`, argv);      
}, o=expected);      

