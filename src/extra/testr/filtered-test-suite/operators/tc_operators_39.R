expected <- eval(parse(text="structure(c(79.3888888888889, 98.8888888888889, 114.222222222222, 123.388888888889), strata = structure(\"Within\", .Names = \"N\"), class = \"mtable\", .Dim = 4L, .Dimnames = structure(list(N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = \"N\"))"));               
test(id=0, code={               
argv <- eval(parse(text="list(structure(c(-24.5833333333333, -5.08333333333333, 10.25, 19.4166666666667), .Dim = 4L, .Dimnames = structure(list(N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = \"N\"), strata = structure(\"Within\", .Names = \"N\"), class = \"mtable\"), structure(103.972222222222, class = \"mtable\"))"));               
do.call(`+`, argv);               
}, o=expected);               

