expected <- eval(parse(text="structure(13823, class = \"Date\")"));      
test(id=0, code={      
argv <- eval(parse(text="list(structure(c(13823, NA), class = \"Date\"), na.rm = TRUE)"));      
do.call(`max`, argv);      
}, o=expected);      

