expected <- eval(parse(text="structure(list(names = \"zi.si.\", row.names = 1:12, class = \"data.frame\"), .Names = c(\"names\", \"row.names\", \"class\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(zi.si. = c(-2.73014251717135, -2.16787987308811, -1.61026765290054, -1.06093652746977, -0.523224065200069, 0, 0.506450782357207, 0.994479058519472, 1.46306067722175, 1.91173866627745, 2.34053598638487, 2.74985599456053)), .Names = \"zi.si.\", row.names = c(NA, -12L), class = \"data.frame\"))"));               
do.call(`attributes`, argv);               
}, o=expected);               

