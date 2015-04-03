expected <- c(37.949, 34.198)
test(id=21, code={
argv <- structure(list(c(37.9490090935718, 34.1981894015095), digits = 3), .Names = c("", 
"digits"))
do.call('round', argv);
},  o = expected);

