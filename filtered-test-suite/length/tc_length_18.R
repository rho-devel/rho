expected <- eval(parse(text="3L"));                   
test(id=0, code={                   
argv <- eval(parse(text="list(structure(list(object = structure(3.14159265358979, comment = \"Start with pi\"), slots = \"comment\", dataPart = TRUE), .Names = c(\"object\", \"slots\", \"dataPart\")))"));                   
do.call(`length`, argv);                   
}, o=expected);                   

