expected <- eval(parse(text="structure(TRUE, .Names = \"d\")"));   
test(id=0, code={   
argv <- eval(parse(text="list(structure(FALSE, .Names = \"d\"), FALSE)"));   
do.call(`==`, argv);   
}, o=expected);   

