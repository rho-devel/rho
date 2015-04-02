expected <- c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", 
"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", 
"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", 
"B", "B")
test(id=102, code={
argv <- list(c("A", "B"), c(48L, 44L))
do.call('rep', argv);
},  o = expected);

