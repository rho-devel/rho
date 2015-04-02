expected <- eval(parse(text="TRUE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(1:3, .Label = c(\"4\", \"5\", \"6\"), class = \"factor\"))"));       
do.call(`is.atomic`, argv);       
}, o=expected);       

