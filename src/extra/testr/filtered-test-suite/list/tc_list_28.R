expected <- eval(parse(text="list(3.14159265358979, \"C\", NaN, Inf, 1:3, c(0, NA), NA)"));         
test(id=0, code={         
argv <- eval(parse(text="list(3.14159265358979, \"C\", NaN, Inf, 1:3, c(0, NA), NA)"));         
do.call(`list`, argv);         
}, o=expected);         

