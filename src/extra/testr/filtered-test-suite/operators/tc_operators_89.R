expected <- eval(parse(text="structure(TRUE, .Names = \"gridSVG2\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(3.0625, base = 4, lens = 3L, .classes = c(\"R_system_version\", \"package_version\", \"numeric_version\")), structure(3.25, .Names = \"gridSVG2\", base = 4, lens = structure(2L, .Names = \"gridSVG2\"), .classes = \"numeric_version\"))"));          
do.call(`<`, argv);          
}, o=expected);          

