expected <- eval(parse(text="structure(TRUE, .Names = \"gridSVG2\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(c(3L, 0L, 1L)), class = c(\"R_system_version\", \"package_version\", \"numeric_version\")), structure(\"3.1\", .Names = \"gridSVG2\"))"));          
do.call(`<`, argv);          
}, o=expected);          

