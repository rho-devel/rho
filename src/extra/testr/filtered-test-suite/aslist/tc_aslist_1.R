expected <- structure(list(X = 9.83610941897737, Y = 1.76740501065812, Z = 3.23822416444495, 
    a = -2.66666666666667, b = -10, c = 28), .Names = c("X", 
"Y", "Z", "a", "b", "c"))
test(id=12735, code={
argv <- structure(list(x = structure(c(9.83610941897737, 1.76740501065812, 
3.23822416444495, -2.66666666666667, -10, 28), .Names = c("X", 
"Y", "Z", "a", "b", "c"))), .Names = "x")
do.call('as.list', argv);
},  o = expected);

