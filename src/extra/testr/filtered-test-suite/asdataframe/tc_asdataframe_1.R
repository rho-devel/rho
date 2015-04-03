expected <- structure(list(effsize = c(3.5, 2, 1.7), constraint = c(0.40625, 
0.5, 0.882), outdegree = c(4, 2, 2), indegree = c(4, 2, 3), efficiency = c(0.625, 
0.5, 0.444444444444444), hierarchy = c(0, 0, 0.333333333333333
), centralization = c(0.833333333333333, 1, 0.333333333333333
), gden = c(0.5, 0.666666666666667, 0.666666666666667), ego.gden = c(0.166666666666667, 
0, 0.5)), .Names = c("effsize", "constraint", "outdegree", "indegree", 
"efficiency", "hierarchy", "centralization", "gden", "ego.gden"
), row.names = c("q1.csv", "q2.csv", "q3.csv"), class = "data.frame")
test(id=11, code={
argv <- structure(list(x = structure(c(3.5, 2, 1.7, 0.40625, 0.5, 0.882, 
4, 2, 2, 4, 2, 3, 0.625, 0.5, 0.444444444444444, 0, 0, 0.333333333333333, 
0.833333333333333, 1, 0.333333333333333, 0.5, 0.666666666666667, 
0.666666666666667, 0.166666666666667, 0, 0.5), .Dim = c(3L, 9L
), .Dimnames = list(c("q1.csv", "q2.csv", "q3.csv"), c("effsize", 
"constraint", "outdegree", "indegree", "efficiency", "hierarchy", 
"centralization", "gden", "ego.gden")))), .Names = "x")
do.call('as.data.frame', argv);
},  o = expected);

