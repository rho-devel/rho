expected <- eval(parse(text="structure(FALSE, .Names = \"plot\")"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(\"graphics\", .Names = \"plot\"))"));        
do.call(`is.na`, argv);        
}, o=expected);        

