expected <- eval(parse(text="structure(c(TRUE, FALSE, TRUE, TRUE), .Names = c(\"age\", \"strata(ss)\", \"age2\", \"age:strata(ss)\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(structure(c(0, 1, 0, 2), .Names = c(\"age\", \"strata(ss)\", \"age2\", \"age:strata(ss)\")), 1)"));         
do.call(`!=`, argv);         
}, o=expected);         

