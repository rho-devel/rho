expected <- structure(c(-2.13777446721376, 1.17045456767922, 5.85180137819007
), .Dim = c(1L, 3L))
test(id=19368, code={
argv <- structure(list(x = c(-2.13777446721376, 1.17045456767922, 5.85180137819007
)), .Names = "x")
do.call('t', argv);
},  o = expected);

