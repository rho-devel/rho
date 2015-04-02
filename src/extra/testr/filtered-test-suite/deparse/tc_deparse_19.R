expected <- eval(parse(text="c(\"structure(list(Sex = structure(c(2L, 2L, 1L, 1L, 2L, 2L), .Label = c(\\\"Female\\\", \", \"\\\"Male\\\"), class = \\\"factor\\\"), age = c(15, 20, 10, 12, 2, 4), Subject = structure(c(2L, \", \"2L, 1L, 1L, 3L, 3L), .Label = c(\\\"F30\\\", \\\"M01\\\", \\\"M04\\\"), class = \\\"factor\\\")), .Names = c(\\\"Sex\\\", \", \"\\\"age\\\", \\\"Subject\\\"), row.names = c(NA, -6L), class = \\\"data.frame\\\")\")"));          
test(id=0, code={          
argv <- eval(parse(text="list(structure(list(Sex = structure(c(2L, 2L, 1L, 1L, 2L, 2L), .Label = c(\"Female\", \"Male\"), class = \"factor\"), age = c(15, 20, 10, 12, 2, 4), Subject = structure(c(2L, 2L, 1L, 1L, 3L, 3L), .Label = c(\"F30\", \"M01\", \"M04\"), class = \"factor\")), .Names = c(\"Sex\", \"age\", \"Subject\"), row.names = c(NA, -6L), class = \"data.frame\"), 60L, FALSE, 69, -1L)"));          
.Internal(deparse(argv[[1]], argv[[2]], argv[[3]], argv[[4]], argv[[5]]));          
}, o=expected);          

