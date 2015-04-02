expected <- eval(parse(text="structure(list(names = c(\"tau\", \"par.vals\")), .Names = \"names\")"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(list(tau = c(-0.704193760852047, 0, 1.5847914530377, 2.07658624888165, 2.62779840842982, 3.16900609499152, 3.70430313207003), par.vals = structure(c(1.19410356771918, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 0.0145810141529953, 0.24263452560295, 0.470688037052905, 0.562956252821107, 0.683253495496408, 0.823187854524599, 0.98897386701965), .Dim = c(7L, 2L), .Dimnames = list(NULL, c(\"a\", \"b\")))), .Names = c(\"tau\", \"par.vals\")))"));               
do.call(`attributes`, argv);               
}, o=expected);               

