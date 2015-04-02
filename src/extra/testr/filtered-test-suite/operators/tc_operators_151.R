expected <- eval(parse(text="TRUE"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(3.00390625, base = 16, lens = 3L, .classes = c(\"R_system_version\", \"package_version\", \"numeric_version\")), structure(2.9375, base = 16, lens = 3L, .classes = c(\"package_version\", \"numeric_version\")))"));          
do.call(`>=`, argv);          
}, o=expected);          

