expected <- eval(parse(text="structure(list(names = c(\"message\", \"call\")), .Names = \"names\")"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(message = \"NAs produced\", call = quote(rnorm(2, numeric()))), .Names = c(\"message\", \"call\")))"));       
do.call(`attributes`, argv);       
}, o=expected);       

