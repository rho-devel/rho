expected <- eval(parse(text="\"4\""));                 
test(id=0, code={                 
argv <- eval(parse(text="list(structure(list(4L), class = c(\"package_version\", \"numeric_version\")))"));                 
do.call(`as.character`, argv);                 
}, o=expected);                 

