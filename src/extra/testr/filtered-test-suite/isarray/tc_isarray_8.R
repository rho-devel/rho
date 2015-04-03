expected <- eval(parse(text="FALSE"));            
test(id=0, code={            
argv <- eval(parse(text="list(structure(list(breaks = c(26, 30, 54, 25, 70, 52, 51, 26, 67, 27, 14, 29, 19, 29, 31, 41, 20, 44), wool = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Label = c(\"A\", \"B\"), class = \"factor\"), tension = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L), .Label = c(\"L\", \"M\", \"H\"), class = \"factor\")), .Names = c(\"breaks\", \"wool\", \"tension\"), row.names = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L), class = \"data.frame\"))"));            
do.call(`is.array`, argv);            
}, o=expected);            

