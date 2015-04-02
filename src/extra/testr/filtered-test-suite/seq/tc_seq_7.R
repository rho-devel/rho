expected <- eval(parse(text="1:2"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(levels = c(\"1\", \"2\", NA), class = \"factor\"), .Names = c(\"levels\", \"class\")))"));               
do.call(`seq_along`, argv);               
}, o=expected);               

