expected <- eval(parse(text="structure(list(x = 2.28125, y = 1.70580465116279, xlab = NULL, ylab = NULL), .Names = c(\"x\", \"y\", \"xlab\", \"ylab\"))"));         
test(id=0, code={         
argv <- eval(parse(text="list(x = 2.28125, y = 1.70580465116279, xlab = NULL, ylab = NULL)"));         
do.call(`list`, argv);         
}, o=expected);         

