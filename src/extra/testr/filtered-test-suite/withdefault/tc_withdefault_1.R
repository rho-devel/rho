expected <- list(c(-59.7902472994399, 16.8246836854792, -4.8326538055996))
test(id=10357, code={
argv <- structure(list(data = structure(list(X = 22.1693750707316, Y = -0.652127930273561, 
    Z = 1.03034043827436, a = -2.66666666666667, b = -10, c = 28), .Names = c("X", 
"Y", "Z", "a", "b", "c")), expr = expression({
    dX <- a * X + Y * Z
    dY <- b * (Y - Z)
    dZ <- -X * Y + c * Y - Z
    list(c(dX, dY, dZ))
})), .Names = c("data", "expr"))
do.call('with.default', argv);
},  o = expected);

