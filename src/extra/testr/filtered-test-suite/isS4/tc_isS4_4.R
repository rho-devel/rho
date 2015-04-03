expected <- eval(parse(text="FALSE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(c(\"a1\", NA, NA, \"a4\"), class = \"AsIs\"))"));          
do.call(`isS4`, argv);          
}, o=expected);          

