expected <- eval(parse(text="quote(Fr ~ (Hair + Eye + Sex)^2)"));        
test(id=0, code={        
argv <- eval(parse(text="list(structure(list(Fr = c(32, 53, 10, 3, 11, 50, 10, 30, 10, 25, 7, 5, 3, 15, 7, 8, 36, 66, 16, 4, 9, 34, 7, 64, 5, 29, 7, 5, 2, 14, 7, 8), Hair = structure(c(1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L, 1L, 2L, 3L, 4L), .Label = c(\"Black\", \"Brown\", \"Red\", \"Blond\"), class = \"factor\"), Eye = structure(c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L), .Label = c(\"Brown\", \"Blue\", \"Hazel\", \"Green\"), class = \"factor\"), Sex = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c(\"Male\", \"Female\"), class = \"factor\")), .Names = c(\"Fr\", \"Hair\", \"Eye\", \"Sex\"), terms = quote(Fr ~ (Hair + Eye + Sex)^2), row.names = c(NA, 32L), class = \"data.frame\"), \"terms\")"));        
do.call(`attr`, argv);        
}, o=expected);        

