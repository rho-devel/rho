expected <- c(0-2i, 0-2i, 0-2i, 0-2i, 0-2i, 0-2i, 0-2i, 0-2i, 0-2i, 0-2i, 
0-2i, 0-2i, 0-2i)
test(id=52, code={
argv <- list(0-2i, 13)
do.call('rep', argv);
},  o = expected);

