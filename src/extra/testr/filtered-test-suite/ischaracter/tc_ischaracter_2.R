expected <- eval(parse(text="FALSE"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(list(mai = c(0.51, 0.41, 0.41, 0.21), mar = c(5.1, 4.1, 4.1, 2.1), cex = 1, yaxs = \"r\"), .Names = c(\"mai\", \"mar\", \"cex\", \"yaxs\")))"));   
do.call(`is.character`, argv);   
}, o=expected);   

