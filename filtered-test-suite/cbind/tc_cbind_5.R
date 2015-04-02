expected <- structure(c(1, 223, 312, 712, 889, 1201, 1467, 222, 311, 711, 
888, 1200, 1466, 1600), .Dim = c(7L, 2L), .Dimnames = list(c("", 
"1th break", "2th break", "3th break", "4th break", "5th break", 
"6th break"), NULL))
test(id=260, code={
argv <- list(structure(c(1, 223, 312, 712, 889, 1201, 1467), .Names = c("", 
"1th break", "2th break", "3th break", "4th break", "5th break", 
"6th break")), structure(c(222, 311, 711, 888, 1200, 1466, 1600
), .Names = c("1th break", "2th break", "3th break", "4th break", 
"5th break", "6th break", "7th break")))
do.call('cbind', argv);
},  o = expected);

