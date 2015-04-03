expected <- eval(parse(text="FALSE"));       
test(id=0, code={       
argv <- eval(parse(text="list(structure(list(f = structure(c(1L, 1L, 1L), .Label = c(\"1\", \"2\"), class = \"factor\"), u = structure(12:14, unit = \"kg\", class = \"avector\")), .Names = c(\"f\", \"u\"), row.names = 2:4, class = \"data.frame\"))"));       
do.call(`is.pairlist`, argv);       
}, o=expected);       

