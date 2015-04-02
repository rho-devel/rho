expected <- TRUE             
test(id=1444, code={             
argv <- structure(list(target = structure(c(1L, 2L, 3L, 4L, 5L, 6L, 7L,              
8L, 9L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 3L, 4L, 5L, 6L,              
7L, 8L, 9L, 10L, 11L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L), .Dim = c(9L,              
4L), .Dimnames = list(c("c.1", "c.2", "c.3", "c.4", "c.5", "c.6",              
"c.7", "c.8", "c.9"), c("A", "B", "C", "D"))), current = structure(c(1,              
2, 3, 4, 5, 6, 7, 8, 9, 2.00000000000001, 3, 4, 5, 6, 7, 8, 9,              
10, 3.00000000000001, 4, 5, 6, 7, 8, 9, 10, 11, 4.00000000000001,              
5, 6, 7, 8, 9, 10, 11, 12), .Dim = c(9L, 4L), .Dimnames = list(             
    c("c.1", "c.2", "c.3", "c.4", "c.5", "c.6", "c.7", "c.8",              
    "c.9"), c("A", "B", "C", "D"))), tolerance = 1e-12), .Names = c("target",              
"current", "tolerance"))             
do.call('all.equal.numeric', argv);             
},  o = expected);             
             
