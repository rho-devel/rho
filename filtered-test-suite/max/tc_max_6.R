expected <- eval(parse(text="10L"));              
test(id=0, code={              
argv <- eval(parse(text="list(1L, structure(1:10, .Label = c(\"a\", \"b\", \"c\", \"d\", \"e\", \"f\", \"g\", \"h\", \"i\", \"j\"), class = \"factor\"))"));              
do.call(`max`, argv);              
}, o=expected);              

