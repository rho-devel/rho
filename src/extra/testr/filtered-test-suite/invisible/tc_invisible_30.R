expected <- eval(parse(text="structure(list(), class = \"formula\")"));             
test(id=0, code={             
argv <- eval(parse(text="list(structure(list(), class = \"formula\"))"));             
do.call(`invisible`, argv);             
}, o=expected);             

