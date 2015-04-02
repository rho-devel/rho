expected <- eval(parse(text="quote(cbind(X, M) ~ M.user + Temp + Soft)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(`cbind(X, M)` = structure(c(68, 42, 37, 24, 66, 33, 47, 23, 63, 29, 57, 19, 42, 30, 52, 43, 50, 23, 55, 47, 53, 27, 49, 29), .Dim = c(12L, 2L), .Dimnames = list(NULL, c(\"X\", \"M\"))), M.user = structure(c(1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L, 1L, 1L, 2L, 2L), .Label = c(\"N\", \"Y\"), class = \"factor\"), Temp = structure(c(2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L, 2L, 1L), .Label = c(\"High\", \"Low\"), class = \"factor\"), Soft = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L), .Label = c(\"Hard\", \"Medium\", \"Soft\"), class = \"factor\")), .Names = c(\"cbind(X, M)\", \"M.user\", \"Temp\", \"Soft\"), terms = quote(cbind(X, M) ~ M.user + Temp + Soft), row.names = c(\"1\", \"3\", \"5\", \"7\", \"9\", \"11\", \"13\", \"15\", \"17\", \"19\", \"21\", \"23\"), class = \"data.frame\"), \"terms\")"));        
do.call(`attr`, argv);        
}, o=expected);        

