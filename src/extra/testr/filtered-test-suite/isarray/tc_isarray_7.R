expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(\"2001-01-01\", NA, NA, \"2004-10-26\"))"));            
do.call(`is.array`, argv);            
}, o=expected);            

