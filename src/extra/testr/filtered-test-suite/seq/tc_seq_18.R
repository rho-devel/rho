expected <- 1:2     
test(id=2, code={     
argv <- list(c(2L, 2L))     
do.call('seq_len', argv);     
}, w = "first element used of 'length.out' argument", o = expected);     
     
