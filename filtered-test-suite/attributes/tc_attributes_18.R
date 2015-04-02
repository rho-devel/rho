expected <- eval(parse(text="structure(list(comment = \"Start with pi\"), .Names = \"comment\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(3.14159265358979, comment = \"Start with pi\"))"));               
do.call(`attributes`, argv);               
}, o=expected);               

