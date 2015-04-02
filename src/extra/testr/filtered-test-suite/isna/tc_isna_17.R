expected <- eval(parse(text="structure(FALSE, .Names = \"value\")"));                
test(id=0, code={                
argv <- eval(parse(text="list(structure(0.0129709025545593, .Names = \"value\"))"));                
do.call(`is.na`, argv);                
}, o=expected);                

