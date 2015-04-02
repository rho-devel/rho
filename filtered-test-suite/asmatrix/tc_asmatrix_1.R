expected <- structure(c(9L, 27L, 27L, 27L, 27L, 3L, 3L, 3L, 3L, 9L, 9L, 9L, 
9L, 9L, 9L), .Dim = c(15L, 1L), .Dimnames = list(c("Blocks", 
"A", "B", "C", "D", "Blocks:A", "Blocks:B", "Blocks:C", "Blocks:D", 
"A:B", "A:C", "A:D", "B:C", "B:D", "C:D"), NULL))
test(id=0, code={
argv <- structure(list(x = structure(c(9L, 27L, 27L, 27L, 27L, 3L, 3L, 
3L, 3L, 9L, 9L, 9L, 9L, 9L, 9L), .Names = c("Blocks", "A", "B", 
"C", "D", "Blocks:A", "Blocks:B", "Blocks:C", "Blocks:D", "A:B", 
"A:C", "A:D", "B:C", "B:D", "C:D"))), .Names = "x")
do.call('as.matrix', argv);
},  o = expected);

