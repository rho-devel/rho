expected <- eval(parse(text="structure(c(NA, FALSE, NA, FALSE, FALSE, FALSE, NA, FALSE, TRUE), .Names = c(NA, \"FALSE\", \"TRUE\", NA, \"FALSE\", \"TRUE\", NA, \"FALSE\", \"TRUE\"))"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(c(NA, FALSE, TRUE, NA, FALSE, TRUE, NA, FALSE, TRUE), .Names = c(NA, \"FALSE\", \"TRUE\", NA, \"FALSE\", \"TRUE\", NA, \"FALSE\", \"TRUE\")), structure(c(NA, NA, NA, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE), .Names = c(NA, NA, NA, \"FALSE\", \"FALSE\", \"FALSE\", \"TRUE\", \"TRUE\", \"TRUE\")))"));       
do.call(`&`, argv);       
}, o=expected);       

