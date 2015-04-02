expected <- eval(parse(text="structure(c(-0.233567190135781, 1.04409752128647), .Names = c(\"Low|Medium\", \"Medium|High\"))"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(c(-0.233567190135781, 1.27766471142225), .Names = c(\"Low|Medium\", \"Medium|High\")))"));             
do.call(`cumsum`, argv);             
}, o=expected);             

