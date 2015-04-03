expected <- eval(parse(text="NA"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(NA, NA, NA, NA, NA, NA, NA, NA), .Names = c(\"base\", \"utils\", \"methods\", \"grDevices\", \"graphics\", \"stats\", \"lapack\", \"R_X11\")))"));             
do.call(`any`, argv);             
}, o=expected);             

