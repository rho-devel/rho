expected <- structure(c(8048, 8066, 8092, 8092, 8093), origin = structure(c(1, 
1, 1970), .Names = c("month", "day", "year")), class = c("dates", 
"times"))
test(id=0, code={
argv <- structure(list(x = structure(c(8092, 8092, 8048, 8093, 8066), origin = structure(c(1, 
1, 1970), .Names = c("month", "day", "year")), class = c("dates", 
"times"))), .Names = "x")
do.call('sort', argv);
},  o = expected);

