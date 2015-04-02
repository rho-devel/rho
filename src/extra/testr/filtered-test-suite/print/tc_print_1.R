expected <- "The leverage of the points is"
test(id=84, code={
argv <- structure(list(x = "The leverage of the points is"), .Names = "x")
do.call('print', argv);
},  o = expected);

