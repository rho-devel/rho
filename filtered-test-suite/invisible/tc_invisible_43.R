expected <- eval(parse(text="structure(c(\"0\", \"NULL\", \"NULL\"), .Names = c(\"Length\", \"Class\", \"Mode\"), class = c(\"summaryDefault\", \"table\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(\"0\", \"NULL\", \"NULL\"), .Names = c(\"Length\", \"Class\", \"Mode\"), class = c(\"summaryDefault\", \"table\")))"));             
do.call(`invisible`, argv);             
}, o=expected);             

