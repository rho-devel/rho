expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(x = 1:10, yyy = 11:20), .Names = c(\"x\", \"yyy\"), row.names = c(NA, -10L), class = \"data.frame\"))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

