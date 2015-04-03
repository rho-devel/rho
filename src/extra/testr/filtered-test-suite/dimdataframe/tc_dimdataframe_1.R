expected <- c(3L, 3L)     
test(id=6, code={     
argv <- structure(list(x = structure(list(c..1....4....7.. = structure(1:3, .Label = c("1",      
"4", "7"), class = "factor"), c..2....5....8.. = structure(1:3, .Label = c("2",      
"5", "8"), class = "factor"), c..3....6....9.. = structure(1:3, .Label = c("3",      
"6", "9"), class = "factor")), .Names = c("c..1....4....7..",      
"c..2....5....8..", "c..3....6....9.."), row.names = c(NA, -3L     
), class = "data.frame")), .Names = "x")     
do.call('dim.data.frame', argv);     
},  o = expected);     
     
