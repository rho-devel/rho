expected <- eval(parse(text="list(1L, 3.14159265358979, 3+5i, \"testit\", TRUE, structure(1L, .Label = \"foo\", class = \"factor\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(1L, 3.14159265358979, 3+5i, \"testit\", TRUE, structure(1L, .Label = \"foo\", class = \"factor\"))"));         
do.call(`list`, argv);         
}, o=expected);         

