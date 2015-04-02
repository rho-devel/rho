expected <- eval(parse(text="structure(\"abc\", class = \"AsIs\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(a = structure(\"abc\", class = \"AsIs\"), b = structure(\"def\\\"gh\", class = \"AsIs\")), .Names = c(\"a\", \"b\"), row.names = \"1\", class = \"data.frame\"), 1L)"));       
do.call(`.subset2`, argv);       
}, o=expected);       

