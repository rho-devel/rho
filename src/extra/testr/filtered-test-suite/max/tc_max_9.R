expected <- eval(parse(text="1"));              
test(id=0, code={              
argv <- eval(parse(text="list(structure(list(x = c(-1, 1, 1, -1, -1, 1, 1, -1), y = c(-0.701149425287356, -0.701149425287356, -0.701149425287356, -0.701149425287356, 0.701149425287356, 0.701149425287356, 0.701149425287356, 0.701149425287356), z = c(-0.4, -0.4, 0.4, 0.4, -0.4, -0.4, 0.4, 0.4)), .Names = c(\"x\", \"y\", \"z\"), row.names = c(NA, -8L), class = \"data.frame\"), na.rm = FALSE)"));              
do.call(`max`, argv);              
}, o=expected);              

