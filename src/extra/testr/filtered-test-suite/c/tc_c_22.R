expected <- eval(parse(text="structure(list(N = structure(c(17, 18, 18, 18), .Dim = 4L, .Dimnames = structure(list(N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = \"N\")), `V:N` = structure(c(6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6), .Dim = 3:4, .Dimnames = structure(list(V = c(\"Golden.rain\", \"Marvellous\", \"Victory\"), N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = c(\"V\", \"N\")))), .Names = c(\"N\", \"V:N\"))"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(N = structure(c(17, 18, 18, 18), .Dim = 4L, .Dimnames = structure(list(N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = \"N\"))), .Names = \"N\"), structure(list(`V:N` = structure(c(6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6), .Dim = 3:4, .Dimnames = structure(list(V = c(\"Golden.rain\", \"Marvellous\", \"Victory\"), N = c(\"0.0cwt\", \"0.2cwt\", \"0.4cwt\", \"0.6cwt\")), .Names = c(\"V\", \"N\")))), .Names = \"V:N\"))"));        
do.call(`c`, argv);        
}, o=expected);        

