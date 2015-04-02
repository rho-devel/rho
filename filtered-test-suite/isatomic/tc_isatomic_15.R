expected <- eval(parse(text="FALSE"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(list(age = 62.4473684210526, age.strata.sex.sex.2 = 24.109649122807), .Names = c(\"age\", \"age.strata.sex.sex.2\"), row.names = c(NA, -1L), class = \"data.frame\"))"));                
do.call(`is.atomic`, argv);                
}, o=expected);                

