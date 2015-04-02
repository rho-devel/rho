expected <- eval(parse(text="structure(list(value = 4.94065645841247e-324, visible = TRUE), .Names = c(\"value\", \"visible\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(value = 4.94065645841247e-324, visible = TRUE), .Names = c(\"value\", \"visible\")))"));             
do.call(`invisible`, argv);             
}, o=expected);             

