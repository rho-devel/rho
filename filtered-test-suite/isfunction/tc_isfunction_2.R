expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(c(\"a\", \"b\", NA, NA, NA, \"f\", \"g\", \"h\", \"i\", \"j\", \"k\", \"l\"))"));            
do.call(`is.function`, argv);            
}, o=expected);            

