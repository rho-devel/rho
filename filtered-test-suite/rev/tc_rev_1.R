expected <- c("#00FF00FF", "#FFFF00FF", "#FF0000FF")
test(id=0, code={
argv <- structure(list(x = c("#FF0000FF", "#FFFF00FF", "#00FF00FF")), .Names = "x")
do.call('rev', argv);
},  o = expected);

